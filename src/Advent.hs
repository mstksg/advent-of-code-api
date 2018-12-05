{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeInType         #-}

module Advent (
  -- * API
    AoC(..)
  , AoCSettings(..)
  , SubmitRes(..), showSubmitRes
  , runAoC
  -- * Utility
  -- ** Day
  , mkDay, mkDay_
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
import           Data.Map             (Map)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text            (Text)
import           Data.Typeable
import           Network.Curl
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified System.IO.Unsafe     as Unsafe
import qualified Text.Taggy           as H

initialThrottleLimit :: Int
initialThrottleLimit = 1000

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
  deriving (Show, Eq, Ord)

-- | An API command.  An @'AoC' a@ an AoC API request that returns
-- results of type @a@.
data AoC :: Type -> Type where
    -- | Fetch prompts, as HTML.
    --
    -- Note that you can give a bogus Session Key and still get Part 1 of
    -- a given day, provided the day is already released.
    AoCPrompt
        :: Finite 25             -- ^ Day.
        -> AoC (Map Char Text)   -- ^ Map of prompts (as markdown). Part 1 is under @\'a\'@,
                                 --   Part 2 is under @\'b\'@, etc.
    -- | Fetch input
    AoCInput
        :: Finite 25             -- ^ Day.
        -> AoC Text

    -- | Submit answer.
    AoCSubmit
        :: Finite 25              -- ^ Day.
        -> Char                   -- ^ Part.  \'a\' for part 1, \'b\' for part 2, etc.
        -> String                 -- ^ Answer.  __WARNING__: not escaped or length-limited.
        -> AoC (Text, SubmitRes)  -- ^ Submission reply (as HTML), and result token

deriving instance Show (AoC a)

-- | A possible (syncronous, logical, pure) error returnable from 'runAoC'.
-- Does not cover any asynchronous or IO errors.
data AoCError
    -- | A libcurl error, with response code and response body
    = AoCCurlError CurlCode String
    -- | The throttler limit is full.  Either make less requests, or adjust
    -- it with 'setAoCThrottleLimit'.
    | AoCThrottleError
  deriving (Show, Typeable)
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
  deriving Show

-- | API endpoint for a given command.
apiUrl :: Integer -> AoC a -> FilePath
apiUrl y = \case
    AoCPrompt i     -> printf "https://adventofcode.com/%04y/day/%d"        y (dNum i)
    AoCInput  i     -> printf "https://adventofcode.com/%04y/day/%d/input"  y (dNum i)
    AoCSubmit i _ _ -> printf "https://adventofcode.com/%04y/day/%d/answer" y (dNum i)
  where
    dNum = (+ 1) . getFinite

-- | Create a cookie option from a session key.
sessionKeyCookie :: String -> CurlOption
sessionKeyCookie = CurlCookie . printf "session=%s"

-- | WARNING: does not escape submission answers or limit their length.
apiCurl :: String -> AoC a -> [CurlOption]
apiCurl sess = \case
    AoCPrompt _       -> sessionKeyCookie sess
                       : method_GET
    AoCInput  _       -> sessionKeyCookie sess
                       : method_GET
    AoCSubmit _ p ans -> sessionKeyCookie sess
                       : CurlPostFields [ printf "level=%d" (partNum p)
                                        , printf "answer=%s" (strip ans)
                                        ]
                       : CurlHttpHeaders ["Content-Type: application/x-www-form-urlencoded"]
                       : method_POST
  where
    partNum p = ord p - ord 'a' + 1

apiCache
    :: Maybe String           -- ^ session key
    -> AoC a
    -> Maybe FilePath
apiCache sess = \case
    AoCPrompt d -> Just $ printf "prompt/%02d.yaml" (dNum d)
    AoCInput  d -> Just $ printf "input/%s%02d.yaml" keyDir (dNum d)
    AoCSubmit{} -> Nothing
  where
    dNum = (+ 1) . getFinite
    keyDir = case sess of
      Nothing -> ""
      Just s  -> strip s ++ "/"

-- | Run an 'API' command with a given 'AoCSettings' to produce the result
-- or a list of (lines of) errors.
--
-- __WARNING__: Does not escape submission answers or limit their length,
-- for 'ASubmit'.
runAoC :: AoCSettings -> AoC a -> IO (Either AoCError a)
runAoC AoCSettings{..} a = do
    (keyMayb, cacheDir) <- case _aCache of
      Just c  -> pure (Nothing, c)
      Nothing -> (Just _aSessionKey,) . (</> "advent-of-code-api") <$> getTemporaryDirectory

    let cacher = case apiCache keyMayb a of
          Nothing -> id
          Just fp -> cacheing (cacheDir </> fp) sl

    cacher . withCurlDo . runExceptT $ do
      (cc, r) <- (maybe (throwError AoCThrottleError) pure =<<) 
               . liftIO
               . throttling aocThrottler _aThrottle
               $ curlGetString u (apiCurl _aSessionKey a)
      case cc of
        CurlOK -> return ()
        _      -> throwError $ AoCCurlError cc r
      pure $ processAoC a r
  where
    u = apiUrl _aYear a
    sl = case a of
      AoCPrompt{} -> SL { _slSave = either (const Nothing) Just
                        , _slLoad = Just . Right
                        }
      AoCInput{}  -> SL { _slSave = either (const Nothing) Just
                        , _slLoad = Just . Right
                        }
      AoCSubmit{} -> SL { _slSave = const $ Nothing @()
                        , _slLoad = const Nothing
                        }

-- | Process a string response into the type desired.
processAoC :: AoC a -> String -> a
processAoC = \case
    AoCPrompt{} -> M.fromList . zip ['a'..] . processHTML
    AoCInput{}  -> T.pack
    AoCSubmit{} -> (\o -> (o, parseSubmitRes o))
                 . fromMaybe ""
                 . listToMaybe
                 . processHTML

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
mkDay :: Integer -> Maybe (Finite 25)
mkDay = packFinite . subtract 1

-- | Construct a @'Finite' 25@ (the type of a Day) from a day
-- integer (1 - 25).  Is undefined if input is out of range.  Can be useful
-- for compile-time literals, like @'mkDay_' 4@
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

-- eitherToMaybe :: Either e a -> Maybe a
-- eitherToMaybe = either (const Nothing) Just

strip :: String -> String
strip = T.unpack . T.strip . T.pack
