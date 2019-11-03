{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE ViewPatterns       #-}

-- |
-- Module      : Advent
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell bindings for Advent of Code 2018 API.  Caches and throttles
-- requests automatically.
--
-- Specify your requests with 'AoC' and 'AoCOpts', and run them with
-- 'runAoC'.
--
-- Examples:
--
-- @
-- -- Fetch prompts for day 5
-- 'runAoC' myOpts $ 'AoCPrompt' ('mkDay_' 5)
--
-- -- Fetch input for day 8
-- 'runAoC' myOpts $ 'AoCInput' ('mkDay_' 8)
--
-- -- Submit answer "hello" for Day 10, Part 1
-- 'runAoC' myOpts $ 'AoCSubmit' ('mkDay_' 10) 'Part1' "hello"
-- @
--
-- Please use responsibly.  All actions are by default rate limited to one
-- per three seconds, but this can be adjusted to a hard-limited cap of one
-- per second.
--
-- Note that leaderboard API is not yet supported.

module Advent (
  -- * API
    AoC(..)
  , Part(..)
  , AoCOpts(..)
  , SubmitRes(..), showSubmitRes
  , runAoC
  , defaultAoCOpts
  , AoCError(..)
  -- ** Calendar
  , challengeReleaseTime
  , timeToRelease
  , challengeReleased
  -- * Utility
  -- ** Day
  , mkDay, mkDay_, dayInt
  , aocDay
  -- ** Part
  , partChar, partInt
  -- ** Throttler
  , setAoCThrottleLimit, getAoCThrottleLimit
  -- * Internal
  , parseSubmitRes
  , processHTML
  ) where

-- import           Network.HTTP.Client
-- import qualified Network.URI.Encode   as URI
import           Advent.API
import           Advent.Cache
import           Advent.Throttle
import           Control.Applicative
import           Control.Exception
import           Control.Monad.Except
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Kind
import           Data.Map                (Map)
import           Data.Maybe
import           Data.Set                (Set)
import           Data.Text               (Text)
import           Data.Time
import           Data.Typeable
import           GHC.Generics            (Generic)
import           Network.Curl
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           System.Directory
import           System.FilePath
import           Text.HTML.TagSoup.Tree  (TagTree(..))
import           Text.Printf
import qualified Data.Attoparsec.Text    as P
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Text               as T
import qualified System.IO.Unsafe        as Unsafe
import qualified Text.HTML.TagSoup       as H
import qualified Text.HTML.TagSoup.Tree  as H

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

initialThrottleLimit :: Int
initialThrottleLimit = 100

aocThrottler :: Throttler
aocThrottler = Unsafe.unsafePerformIO $ newThrottler initialThrottleLimit
{-# NOINLINE aocThrottler #-}

-- | Set the internal throttler maximum queue capacity.  Default is 100.
setAoCThrottleLimit :: Int -> IO ()
setAoCThrottleLimit = setLimit aocThrottler

-- | Get the internal throttler maximum queue capacity.
getAoCThrottleLimit :: IO Int
getAoCThrottleLimit = getLimit aocThrottler

-- | The result of a submission.
data SubmitRes
    -- | Correct submission, including global rank (if reported, which
    -- usually happens if rank is under 1000)
    = SubCorrect (Maybe Integer)
    -- | Incorrect submission.  Contains the number of /seconds/ you must
    -- wait before trying again.  The 'Maybe' contains possible hints given
    -- by the server (usually "too low" or "too high").
    | SubIncorrect Int (Maybe String)
    -- | Submission was rejected because an incorrect submission was
    -- recently submitted.  Contains the number of /seconds/ you must wait
    -- before trying again.
    | SubWait Int
    -- | Submission was rejected because it was sent to an invalid question
    -- or part.  Usually happens if you submit to a part you have already
    -- answered or have not yet unlocked.
    | SubInvalid
    -- | Could not parse server response.  Contains parse error.
    | SubUnknown String
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

-- | An API command.  An @'AoC' a@ an AoC API request that returns
-- results of type @a@.
--
-- A lot of these commands take @'Finite' 25@, which represents a day of
-- December up to and including Christmas Day (December 25th).  You can
-- convert an integer day (1 - 25) into a @'Finite' 25@ representing that
-- day using 'mkDay' or 'mkDay_'.
data AoC :: Type -> Type where
    -- | Fetch prompts for a given day.  Returns a 'Map' of 'Part's and
    -- their associated promps, as HTML.
    AoCPrompt
        :: Finite 25
        -> AoC (Map Part Text)

    -- | Fetch input, as plaintext.  Returned verbatim.  Be aware that
    -- input might contain trailing newlines.
    AoCInput :: Finite 25 -> AoC Text

    -- | Submit a plaintext answer (the 'String') to a given day and part.
    -- Receive a server reponse (as HTML) and a response code 'SubmitRes'.
    --
    -- __WARNING__: Answers are not length-limited.  Answers are stripped
    -- of leading and trailing whitespace and run through 'URI.encode'
    -- before submitting.
    AoCSubmit
        :: Finite 25
        -> Part
        -> String
        -> AoC (Text, SubmitRes)

deriving instance Show (AoC a)
deriving instance Typeable (AoC a)

-- | Get the day associated with a given API command.
aocDay :: AoC a -> Finite 25
aocDay (AoCPrompt d    ) = d
aocDay (AoCInput  d    ) = d
aocDay (AoCSubmit d _ _) = d

-- | A possible (syncronous, logical, pure) error returnable from 'runAoC'.
-- Does not cover any asynchronous or IO errors.
data AoCError
    -- | A libcurl error, with response code and response body
    = AoCCurlError CurlCode String
    -- | An error in the http request itself
    | AoCClientError ClientError
    -- | Tried to interact with a challenge that has not yet been
    -- released.  Contains the amount of time until release.
    | AoCReleaseError NominalDiffTime
    -- | The throttler limit is full.  Either make less requests, or adjust
    -- it with 'setAoCThrottleLimit'.
    | AoCThrottleError
  deriving (Show, Typeable, Generic)
instance Exception AoCError

-- | Setings for running an API request.
--
-- Session keys are required for all commands, but if you enter a bogus key
-- you should be able to get at least Part 1 from 'AoCPrompt'.
--
-- The session key can be found by logging in on a web client and checking
-- the cookies.  You can usually check these with in-browser developer
-- tools.
--
-- Throttling is hard-limited to a minimum of 1 second between calls.
-- Please be respectful and do not try to bypass this.
data AoCOpts = AoCOpts
    { -- | Session key
      _aSessionKey :: String
      -- | Year of challenge
    , _aYear       :: Integer
      -- | Cache directory.  If 'Nothing' is given, one will be allocated
      -- using 'getTemporaryDirectory'.
    , _aCache      :: Maybe FilePath
      -- | Fetch results even if cached.  Still subject to throttling.
      -- Default is False.
    , _aForce      :: Bool
      -- | Throttle delay, in milliseconds.  Minimum is 1000000.  Default
      -- is 3000000 (3 seconds).
    , _aThrottle   :: Int
      -- | (Low-level usage) Extra 'CurlOption' options to feed to the
      -- libcurl bindings.  Meant for things like proxy options and custom
      -- SSL certificates.  You should normally not have to add anything
      -- here, since the library manages cookies, request methods, etc. for
      -- you.  Anything other than tweaking low-level network options (like
      -- the ones mentioned previously) will likely break everything.
      -- Default is @[]@.
    , _aCurlOpts   :: [CurlOption]
    }
  deriving (Show, Typeable, Generic)

-- | Sensible defaults for 'AoCOpts' for a given year and session key.
--
-- Use system temporary directory as cache, and throttle requests to one
-- request per three seconds.
defaultAoCOpts
    :: Integer
    -> String
    -> AoCOpts
defaultAoCOpts y s = AoCOpts
    { _aSessionKey = s
    , _aYear       = y
    , _aCache      = Nothing
    , _aForce      = False
    , _aThrottle   = 3000000
    , _aCurlOpts   = []
    }

-- -- | API endpoint for a given command.
-- apiUrl :: Integer -> AoC a -> FilePath
-- apiUrl y = \case
--     AoCPrompt i     -> printf "https://adventofcode.com/%04d/day/%d"        y (dayInt i)
--     AoCInput  i     -> printf "https://adventofcode.com/%04d/day/%d/input"  y (dayInt i)
--     AoCSubmit i _ _ -> printf "https://adventofcode.com/%04d/day/%d/answer" y (dayInt i)

-- -- | Create a cookie option from a session key.
-- sessionKeyCookie :: String -> CurlOption
-- sessionKeyCookie = CurlCookie . printf "session=%s"

aocBase :: BaseUrl
aocBase = BaseUrl Https "adventofcode.com" 80 ""

apiReq :: String -> Integer -> AoC a -> ClientM Text
apiReq sess y = \case
    AoCPrompt i       -> let r :<|> _        = adventAPIClient (Just (Session sess)) y (Day i) in r
    AoCInput  i       -> let _ :<|> r :<|> _ = adventAPIClient (Just (Session sess)) y (Day i) in r
    AoCSubmit i p ans -> let _ :<|> _ :<|> r = adventAPIClient (Just (Session sess)) y (Day i)
                         in  r (SubmitInfo p ans)

-- apiCurl :: String -> AoC a -> [CurlOption]
-- apiCurl sess = \case
--     AoCPrompt _       -> sessionKeyCookie sess
--                        : method_GET
--     AoCInput  _       -> sessionKeyCookie sess
--                        : method_GET
--     AoCSubmit _ p ans -> sessionKeyCookie sess
--                        : CurlPostFields [ printf "level=%d"  (partInt p)
--                                         , printf "answer=%s" (enc ans  )
--                                         ]
--                        : CurlHttpHeaders ["Content-Type: application/x-www-form-urlencoded"]
--                        : method_POST
--   where
--     enc = URI.encode . strip

-- | Cache file for a given 'AoC' command
apiCache
    :: Maybe String           -- ^ session key
    -> Integer                -- ^ year
    -> AoC a
    -> Maybe FilePath
apiCache sess yr = \case
    AoCPrompt d -> Just $ printf "prompt/%04d/%02d.html"        yr (dayInt d)
    AoCInput  d -> Just $ printf "input/%s%04d/%02d.txt" keyDir yr (dayInt d)
    AoCSubmit{} -> Nothing
  where
    keyDir = case sess of
      Nothing -> ""
      Just s  -> strip s ++ "/"

-- | Run an 'AoC' command with a given 'AoCOpts' to produce the result
-- or a list of (lines of) errors.
--
-- __WARNING__: Answers are not length-limited.  Answers are stripped
-- of leading and trailing whitespace and run through 'URI.encode'
-- before submitting.
runAoC :: AoCOpts -> AoC a -> IO (Either AoCError a)
runAoC AoCOpts{..} a = do
    (keyMayb, cacheDir) <- case _aCache of
      Just c  -> pure (Nothing, c)
      Nothing -> (Just _aSessionKey,) . (</> "advent-of-code-api") <$> getTemporaryDirectory

    let cacher = case apiCache keyMayb _aYear a of
          Nothing -> id
          Just fp -> cacheing (cacheDir </> fp) $
                       if _aForce
                         then noCache
                         else saverLoader a

    mgr <- newTlsManager
    cacher . withCurlDo . runExceptT $ do
      rel <- liftIO $ timeToRelease _aYear (aocDay a)
      when (rel > 0) $
        throwError $ AoCReleaseError rel

      r <- fmap T.unpack
         . (either (throwError . AoCClientError) pure =<<)
         . (maybe  (throwError AoCThrottleError) pure =<<)
         . liftIO
         . throttling aocThrottler (max 1000000 _aThrottle)
         $ runClientM
             (apiReq _aSessionKey _aYear a)
             (mkClientEnv mgr aocBase)

      pure $ processAoC a r

-- | Process a string response into the type desired.
processAoC :: AoC a -> String -> a
processAoC = \case
    AoCPrompt _ -> M.fromList
                 . zip [Part1 ..]
                 . processHTML
    AoCInput{}  -> T.pack
    AoCSubmit{} -> (\o -> (o, parseSubmitRes o))
                 . fromMaybe ""
                 . listToMaybe
                 . processHTML

-- | Process an HTML webpage into a list of all contents in <article>s
processHTML :: String -> [Text]
processHTML = map H.renderTree
            . mapMaybe isArticle
            . H.universeTree
            . H.parseTree
            . T.pack
  where
    isArticle :: TagTree Text -> Maybe [TagTree Text]
    isArticle (TagBranch n _ ts) = ts <$ guard (n == "article")
    isArticle _                  = Nothing

-- | Parse 'Text' into a 'SubmitRes'.
parseSubmitRes :: Text -> SubmitRes
parseSubmitRes = either SubUnknown id
               . P.parseOnly choices
               . mconcat
               . mapMaybe deTag
               . H.parseTags
  where
    deTag (H.TagText t) = Just t
    deTag _             = Nothing
    choices             = asum [ parseCorrect   P.<?> "Correct"
                               , parseIncorrect P.<?> "Incorrect"
                               , parseWait      P.<?> "Wait"
                               , parseInvalid   P.<?> "Invalid"
                               ]
    parseCorrect = do
      _ <- P.manyTill P.anyChar (P.asciiCI "that's the right answer") P.<?> "Right answer"
      r <- optional . (P.<?> "Rank") $ do
        P.manyTill P.anyChar (P.asciiCI "rank")
          *> P.skipMany (P.satisfy (not . isDigit))
        P.decimal
      pure $ SubCorrect r
    parseIncorrect = do
      _ <- P.manyTill P.anyChar (P.asciiCI "that's not the right answer") P.<?> "Not the right answer"
      hint <- optional . (P.<?> "Hint") $ do
        P.manyTill P.anyChar "your answer is" *> P.skipSpace
        P.takeWhile1 (/= '.')
      P.manyTill P.anyChar (P.asciiCI "wait") *> P.skipSpace
      waitAmt <- (1 <$ P.asciiCI "one") <|> P.decimal
      pure $ SubIncorrect (waitAmt * 60) (T.unpack <$> hint)
    parseWait = do
      _ <- P.manyTill P.anyChar (P.asciiCI "an answer too recently") P.<?> "An answer too recently"
      P.skipMany (P.satisfy (not . isDigit))
      m <- optional . (P.<?> "Delay minutes") $
              P.decimal <* P.char 'm' <* P.skipSpace
      s <- P.decimal <* P.char 's' P.<?> "Delay seconds"
      pure . SubWait $ maybe 0 (* 60) m + s
    parseInvalid = SubInvalid <$ P.manyTill P.anyChar (P.asciiCI "solving the right level")

-- | Pretty-print a 'SubmitRes'
showSubmitRes :: SubmitRes -> String
showSubmitRes = \case
    SubCorrect Nothing    -> "Correct"
    SubCorrect (Just r)   -> printf "Correct (Rank %d)" r
    SubIncorrect i Nothing  -> printf "Incorrect (%d minute wait)" (i `div` 60)
    SubIncorrect i (Just h) -> printf "Incorrect (%s) (%d minute wait)" h (i `div` 60)
    SubWait i             -> let (m,s) = i `divMod` 60
                             in   printf "Wait (%d min %d sec wait)"  m s
    SubInvalid            -> "Invalid"
    SubUnknown r          -> printf "Unknown (%s)" r

saverLoader :: AoC a -> SaverLoader (Either AoCError a)
saverLoader = \case
    AoCPrompt d -> SL { _slSave = either (const Nothing) (Just . encodeMap)
                      , _slLoad = \str ->
                          let mp = decodeMap str
                              hasAll = S.null (expectedParts d `S.difference` M.keysSet mp)
                          in  Right mp <$ guard hasAll
                      }
    AoCInput{}  -> SL { _slSave = either (const Nothing) Just
                      , _slLoad = Just . Right
                      }
    AoCSubmit{} -> noCache
  where
    expectedParts :: Finite 25 -> Set Part
    expectedParts n
      | n == 24   = S.singleton Part1
      | otherwise = S.fromDistinctAscList [Part1 ..]
    sep = ">>>>>>>>>"
    encodeMap mp = T.intercalate "\n" . concat $
                            [ maybeToList $ M.lookup Part1 mp
                            , [sep]
                            , maybeToList $ M.lookup Part2 mp
                            ]
    decodeMap xs = mkMap Part1 part1 <> mkMap Part2 part2
      where
        (part1, drop 1 -> part2) = span (/= sep) (T.lines xs)
        mkMap p (T.intercalate "\n"->ln)
          | T.null (T.strip ln) = M.empty
          | otherwise           = M.singleton p ln

-- | Construct a @'Finite' 25@ (the type of a Day) from a day
-- integer (1 - 25).  If input is out of range, 'Nothing' is returned.  See
-- 'mkDay_' for an unsafe version useful for literals.
--
-- Inverse of 'dayInt'.
mkDay :: Integer -> Maybe (Finite 25)
mkDay = packFinite . subtract 1

-- | Construct a @'Finite' 25@ (the type of a Day) from a day
-- integer (1 - 25).  Is undefined if input is out of range.  Can be useful
-- for compile-time literals, like @'mkDay_' 4@
--
-- Inverse of 'dayInt'.
mkDay_ :: Integer -> Finite 25
mkDay_ = fromMaybe e . mkDay
  where
    e = errorWithoutStackTrace "Advent.mkDay_: Date out of range (1 - 25)"

-- | Get time until release of a given challenge.
timeToRelease
    :: Integer              -- ^ year
    -> Finite 25            -- ^ day
    -> IO NominalDiffTime
timeToRelease y d = (challengeReleaseTime y d `diffUTCTime`) <$> getCurrentTime

-- | Check if a challenge has been released yet.
challengeReleased
    :: Integer              -- ^ year
    -> Finite 25            -- ^ day
    -> IO Bool
challengeReleased y = fmap (<= 0) . timeToRelease y

-- | Prompt release time
challengeReleaseTime
    :: Integer              -- ^ year
    -> Finite 25            -- ^ day
    -> UTCTime
challengeReleaseTime y d = UTCTime (fromGregorian y 12 (fromIntegral (dayInt d)))
                                   (5 * 60 * 60)

-- | Convert a @'Finite' 25@ day into a day integer (1 - 25).  Inverse of
-- 'mkDay'.
dayInt :: Finite 25 -> Integer
dayInt = (+ 1) . getFinite

-- | Convert a 'Part' to an 'Int'.
partInt :: Part -> Int
partInt Part1 = 1
partInt Part2 = 2

-- | A character associated with a given part.  'Part1' is associated with
-- @\'a\'@, and 'Part2' is associated with @\'b\'@
partChar :: Part -> Char
partChar Part1 = 'a'
partChar Part2 = 'b'

strip :: String -> String
strip = T.unpack . T.strip . T.pack
