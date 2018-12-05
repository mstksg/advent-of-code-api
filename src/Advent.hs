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

-- |
-- Module      : Advent
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Haskell bindings for Advent of Code 2018 REST API.
--
-- Specify your requests with 'AoC' and 'AoCSettings', and run them with
-- 'runAoC'.
--
-- Please use responsibly.  All actions are rate-limited to a minimum of
-- one request per minute.

module Advent (
  -- * API
    AoC(..)
  , AoCSettings(..)
  , SubmitRes(..), showSubmitRes
  , runAoC
  , defaultAoCSettings
  -- ** Calendar
  , challengeReleaseTime
  , timeToRelease
  , challengeReleased
  -- * Utility
  -- ** Day
  , mkDay, mkDay_
  , aocDay
  -- ** Throttler
  , setAoCThrottleLimit, getAoCThrottleLimit
  -- * Internal
  , parseSubmitRes
  ) where

import           Advent.Cache
import           Advent.Throttle
import           Control.Exception
import           Control.Monad.Except
import           Data.Char
import           Data.Finite
import           Data.Kind
import           Data.Maybe
import           Data.Semigroup
import           Data.Set             (Set)
import           Data.Text            (Text)
import           Data.Time
import           Data.Typeable
import           GHC.Generics         (Generic)
import           Network.Curl
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Network.URI.Encode   as URI
import qualified System.IO.Unsafe     as Unsafe
import qualified Text.Taggy           as H

initialThrottleLimit :: Int
initialThrottleLimit = 300

aocThrottler :: Throttler
aocThrottler = Unsafe.unsafePerformIO $ newThrottler initialThrottleLimit
{-# NOINLINE aocThrottler #-}

-- | Set the internal throttler maximum queue capacity.  Default is 1000.
setAoCThrottleLimit :: Int -> IO ()
setAoCThrottleLimit = setLimit aocThrottler

-- | Get the internal throttler maximum queue capacity.
getAoCThrottleLimit :: IO Int
getAoCThrottleLimit = getLimit aocThrottler

-- | The result of a submission.
data SubmitRes = SubCorrect (Maybe Integer)     -- ^ global rank
               | SubIncorrect
               | SubWait
               | SubInvalid
               | SubUnknown
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | An API command.  An @'AoC' a@ an AoC API request that returns
-- results of type @a@.
--
-- A lot of these commands take @'Finite' 25@, which represents a day of
-- December up to and including Christmas Day (December 25th).  You can
-- convert an integer day (1 - 25) into a @'Finite' 25@ representing that
-- day using 'mkDay' or 'mkDay_'.
data AoC :: Type -> Type where
    -- | Fetch prompts, as HTML.
    --
    -- Note that you can give a bogus Session Key and still get Part 1 of
    -- a given day, provided the day is already released.
    AoCPrompt
        :: Finite 25             -- ^ Day.
        -> Char                  -- ^ Part.  \'a\' for part 1, \'b\' for part 2, etc.
        -> AoC Text              -- ^ Prompt (as HTML)

    -- | Fetch input
    AoCInput
        :: Finite 25             -- ^ Day.
        -> AoC Text

    -- | Submit answer.
    --
    -- __WARNING__: Answers are not length-limited.  Answers are stripped
    -- of leading and trailing whitespace and run through 'URI.encode'
    -- before submitting.
    AoCSubmit
        :: Finite 25              -- ^ Day.
        -> Char                   -- ^ Part.  \'a\' for part 1, \'b\' for part 2, etc.
        -> String                 -- ^ Answer.  Is not length-limited.
        -> AoC (Text, SubmitRes)  -- ^ Submission reply (as HTML), and result token

deriving instance Show (AoC a)
deriving instance Typeable (AoC a)

-- | Get the day associated with a given API command.
aocDay :: AoC a -> Finite 25
aocDay (AoCPrompt d _  ) = d
aocDay (AoCInput  d    ) = d
aocDay (AoCSubmit d _ _) = d

-- | A possible (syncronous, logical, pure) error returnable from 'runAoC'.
-- Does not cover any asynchronous or IO errors.
data AoCError
    -- | A libcurl error, with response code and response body
    = AoCCurlError CurlCode String
    -- | Tried to get interact with a challenge that has not yet been
    -- released.  Contains the amount of time until release.
    | AoCReleaseError NominalDiffTime
    -- | A given part was not found for a part.  Contains all of the parts
    -- that were found.
    | AoCPartError (Set Char)
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
--
-- If no cache directory is given, one will be allocated using
-- 'getTemporaryDirectory'.
data AoCSettings = AoCSettings
    { _aSessionKey :: String          -- ^ Session key
    , _aYear       :: Integer         -- ^ Year of challenge
    , _aCache      :: Maybe FilePath  -- ^ Cache directory
    , _aThrottle   :: Int             -- ^ Throttle delay, in milliseconds.  Minimum is 1000000.
    }
  deriving (Show, Typeable, Generic)

-- | Sensible defaults for 'AoCSettings' for a given year and session key.
defaultAoCSettings
    :: Integer
    -> String
    -> AoCSettings
defaultAoCSettings y s = AoCSettings
    { _aSessionKey = s
    , _aYear       = y
    , _aCache      = Nothing
    , _aThrottle   = 3000000
    }

-- | API endpoint for a given command.
apiUrl :: Integer -> AoC a -> FilePath
apiUrl y = \case
    AoCPrompt i _   -> printf "https://adventofcode.com/%04y/day/%d"        y (dayToInt i)
    AoCInput  i     -> printf "https://adventofcode.com/%04y/day/%d/input"  y (dayToInt i)
    AoCSubmit i _ _ -> printf "https://adventofcode.com/%04y/day/%d/answer" y (dayToInt i)

-- | Create a cookie option from a session key.
sessionKeyCookie :: String -> CurlOption
sessionKeyCookie = CurlCookie . printf "session=%s"

apiCurl :: String -> AoC a -> [CurlOption]
apiCurl sess = \case
    AoCPrompt _ _     -> sessionKeyCookie sess
                       : method_GET
    AoCInput  _       -> sessionKeyCookie sess
                       : method_GET
    AoCSubmit _ p ans -> sessionKeyCookie sess
                       : CurlPostFields [ printf "level=%d"  (partNum p)
                                        , printf "answer=%s" (enc ans  )
                                        ]
                       : CurlHttpHeaders ["Content-Type: application/x-www-form-urlencoded"]
                       : method_POST
  where
    enc = URI.encode . strip
    partNum p = ord p - ord 'a' + 1

-- | Cache file for a given 'AoC' command
apiCache
    :: Maybe String           -- ^ session key
    -> AoC a
    -> Maybe FilePath
apiCache sess = \case
    AoCPrompt d p -> Just $ printf "prompt/%02d%c.txt"       (dayToInt d) p
    AoCInput  d   -> Just $ printf "input/%s%02d.txt" keyDir (dayToInt d)
    AoCSubmit{}   -> Nothing
  where
    keyDir = case sess of
      Nothing -> ""
      Just s  -> strip s ++ "/"

-- | Run an 'API' command with a given 'AoCSettings' to produce the result
-- or a list of (lines of) errors.
--
-- __WARNING__: Answers are not length-limited.  Answers are stripped
-- of leading and trailing whitespace and run through 'URI.encode'
-- before submitting.
runAoC :: AoCSettings -> AoC a -> IO (Either AoCError a)
runAoC AoCSettings{..} a = do
    (keyMayb, cacheDir) <- case _aCache of
      Just c  -> pure (Nothing, c)
      Nothing -> (Just _aSessionKey,) . (</> "advent-of-code-api") <$> getTemporaryDirectory

    let cacher = case apiCache keyMayb a of
          Nothing -> id
          Just fp -> cacheing (cacheDir </> fp) sl

    cacher . withCurlDo . runExceptT $ do
      rel <- liftIO $ timeToRelease _aYear (aocDay a)
      when (rel > 0) $
        throwError $ AoCReleaseError rel

      (cc, r) <- (maybe (throwError AoCThrottleError) pure =<<)
               . liftIO
               . throttling aocThrottler (max 1000000 _aThrottle)
               $ curlGetString u (apiCurl _aSessionKey a)
      case cc of
        CurlOK -> return ()
        _      -> throwError $ AoCCurlError cc r
      either throwError pure $ processAoC a r
  where
    u = apiUrl _aYear a
    sl = case a of
      AoCPrompt{} -> SL { _slSave = either (const Nothing) (Just . T.unpack)
                        , _slLoad = Just . Right . T.pack
                        }
      AoCInput{}  -> SL { _slSave = either (const Nothing) (Just . T.unpack)
                        , _slLoad = Just . Right . T.pack
                        }
      AoCSubmit{} -> SL { _slSave = const Nothing
                        , _slLoad = const Nothing
                        }

-- | Process a string response into the type desired.
processAoC :: AoC a -> String -> Either AoCError a
processAoC = \case
    AoCPrompt _ p -> packagePrompt p
                   . M.fromList
                   . zip ['a'..]
                   . processHTML
    AoCInput{}    -> Right . T.pack
    AoCSubmit{}   -> Right
                   . (\o -> (o, parseSubmitRes o))
                   . fromMaybe ""
                   . listToMaybe
                   . processHTML
  where
    packagePrompt p mp = case M.lookup p mp of
      Just pr -> Right pr
      Nothing -> Left $ AoCPartError (M.keysSet mp)

-- | Process an HTML webpage into a list of all contents in <article>s
processHTML :: String -> [T.Text]
processHTML = map (TL.toStrict . TL.unlines . map H.render)
            . mapMaybe isArticle
            . foldMap universe
            . listToMaybe
            . H.parseDOM True
            . TL.pack
  where
    isArticle (H.NodeElement (H.Element{..}))
        = eltChildren <$ guard (eltName == "article")
    isArticle _
        = Nothing

-- | Parse 'Text' into a 'SubmitRes'.
parseSubmitRes :: Text -> SubmitRes
parseSubmitRes t
    | "the right answer!"       `T.isInfixOf` t = SubCorrect $ findRank t
    | "not the right answer."   `T.isInfixOf` t = SubIncorrect
    | "an answer too recently"  `T.isInfixOf` t = SubWait
    | "solving the right level" `T.isInfixOf` t = SubInvalid
    | otherwise                                 = SubUnknown
  where
    findRank = go . T.words . T.map onlyAlphaNum . T.toLower
      where
        go ("rank":n:_) = readMaybe $ T.unpack n
        go (_     :ws ) = go ws
        go []           = Nothing
        onlyAlphaNum c
          | isAlphaNum c = c
          | otherwise    = ' '

-- | Pretty-print a 'SubmitRes'
showSubmitRes :: SubmitRes -> String
showSubmitRes = \case
    SubCorrect Nothing  -> "Correct"
    SubCorrect (Just r) -> printf "Correct (Rank %d)" r
    SubIncorrect        -> "Incorrect"
    SubWait             -> "Wait"
    SubInvalid          -> "Invalid"
    SubUnknown          -> "Unknown"

-- | Construct a @'Finite' 25@ (the type of a Day) from a day
-- integer (1 - 25).  If input is out of range, 'Nothing' is returned.  See
-- 'mkDay_' for an unsafe version useful for literals.
--
-- Inverse of 'dayToInt'.
mkDay :: Integer -> Maybe (Finite 25)
mkDay = packFinite . subtract 1

-- | Construct a @'Finite' 25@ (the type of a Day) from a day
-- integer (1 - 25).  Is undefined if input is out of range.  Can be useful
-- for compile-time literals, like @'mkDay_' 4@
--
-- Inverse of 'dayToInt'.
mkDay_ :: Integer -> Finite 25
mkDay_ = fromMaybe e . mkDay
  where
    e = errorWithoutStackTrace "Advent.mkDay_: Date out of range (1 - 25)"

-- | All nodes within a node.
universe :: H.Node -> [H.Node]
universe = ($ []) . appEndo . go
  where
    go :: H.Node -> Endo [H.Node]
    go (H.NodeElement (H.Element{..})) = Endo (eltChildren ++)
                                      <> foldMap go eltChildren
    go (H.NodeContent _              ) = mempty

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
challengeReleaseTime y d = UTCTime (fromGregorian y 12 (fromIntegral (dayToInt d)))
                                   (5 * 60 * 60)

-- | Convert a @'Finite' 25@ day into a day integer (1 - 25).  Inverse of
-- 'mkDay'.
dayToInt :: Finite 25 -> Integer
dayToInt = (+ 1) . getFinite

strip :: String -> String
strip = T.unpack . T.strip . T.pack
