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
-- Copyright   : (c) Justin Le 2019
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

module Advent (
  -- * API
    AoC(..)
  , Part(..)
  , Day(..)
  , AoCOpts(..)
  , SubmitRes(..), showSubmitRes
  , Leaderboard(..), LeaderboardMember(..)
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
  , aocReq
  , aocBase
  ) where

import           Advent.API
import           Advent.Cache
import           Advent.Throttle
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import           Data.Kind
import           Data.Map                (Map)
import           Data.Maybe
import           Data.Set                (Set)
import           Data.Text               (Text)
import           Data.Time hiding        (Day)
import           Data.Typeable
import           GHC.Generics            (Generic)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Servant.API
import           Servant.Client
import           System.Directory
import           System.FilePath
import           Text.Printf
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified System.IO.Unsafe        as Unsafe

#if MIN_VERSION_base(4,11,0)
import           Data.Functor
#else
import           Data.Semigroup ((<>))

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
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

-- | An API command.  An @'AoC' a@ an AoC API request that returns
-- results of type @a@.
--
-- A lot of these commands take 'Day', which represents a day of December
-- up to and including Christmas Day (December 25th).  You can convert an
-- integer day (1 - 25) into a 'Day' using 'mkDay' or 'mkDay_'.
data AoC :: Type -> Type where
    -- | Fetch prompts for a given day.  Returns a 'Map' of 'Part's and
    -- their associated promps, as HTML.
    AoCPrompt
        :: Day
        -> AoC (Map Part Text)

    -- | Fetch input, as plaintext.  Returned verbatim.  Be aware that
    -- input might contain trailing newlines.
    AoCInput :: Day -> AoC Text

    -- | Submit a plaintext answer (the 'String') to a given day and part.
    -- Receive a server reponse (as HTML) and a response code 'SubmitRes'.
    --
    -- __WARNING__: Answers are not length-limited.  Answers are stripped
    -- of leading and trailing whitespace and run through 'URI.encode'
    -- before submitting.
    AoCSubmit
        :: Day
        -> Part
        -> String
        -> AoC (Text, SubmitRes)

    -- | Fetch the leaderboard for a given leaderboard public code (owner
    -- member ID).  Requires session key.
    --
    -- The public code can be found in the URL of the leaderboard:
    --
    -- > https://adventofcode.com/2019/leaderboard/private/view/12345
    --
    -- (the @12345@ above)
    --
    -- __NOTE__: This is the most expensive and taxing possible API call,
    -- and makes up the majority of bandwidth to the Advent of Code
    -- servers.  As a courtesy to all who are participating in Advent of
    -- Code, please use this super respectfully, especially in December: if
    -- you set up automation for this, please do not use it more than once
    -- per day.
    --
    -- @since 0.2.0.0
    AoCLeaderboard
        :: Integer
        -> AoC Leaderboard
    
    AoCDailyLeaderboard
        :: Day
        -> AoC DailyLeaderboard

deriving instance Show (AoC a)
deriving instance Typeable (AoC a)

-- | Get the day associated with a given API command, if there is one.
aocDay :: AoC a -> Maybe Day
aocDay (AoCPrompt d     ) = Just d
aocDay (AoCInput  d     ) = Just d
aocDay (AoCSubmit d _ _ ) = Just d
aocDay (AoCLeaderboard _) = Nothing
aocDay (AoCDailyLeaderboard d) = Just d

-- | A possible (syncronous, logical, pure) error returnable from 'runAoC'.
-- Does not cover any asynchronous or IO errors.
data AoCError
    -- | An error in the http request itself
    --
    -- Note that if you are building this with servant-client-core <= 0.16,
    -- this will contain @ServantError@ instead of @ClientError@, which was
    -- the previous name of ths type.
#if MIN_VERSION_servant_client_core(0,16,0)
    = AoCClientError ClientError
#else
    = AoCClientError ServantError
#endif
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
    }

-- | HTTPS base of Advent of Code API.
aocBase :: BaseUrl
aocBase = BaseUrl Https "adventofcode.com" 443 ""

-- | 'ClientM' request for a given 'AoC' API call.
aocReq :: Integer -> AoC a -> ClientM a
aocReq yr = \case
    AoCPrompt i       -> let r :<|> _        = adventAPIPuzzleClient yr i in r
    AoCInput  i       -> let _ :<|> r :<|> _ = adventAPIPuzzleClient yr i in r
    AoCSubmit i p ans -> let _ :<|> _ :<|> r = adventAPIPuzzleClient yr i
                         in  r (SubmitInfo p ans) <&> \(x :<|> y) -> (x, y)
    AoCLeaderboard c  -> let _ :<|> _ :<|> r = adventAPIClient yr
                         in  r (PublicCode c)
    AoCDailyLeaderboard d -> let _ :<|> r :<|> _ = adventAPIClient yr
                             in  r d


-- | Cache file for a given 'AoC' command
apiCache
    :: Maybe String           -- ^ session key
    -> Integer                -- ^ year
    -> AoC a
    -> Maybe FilePath
apiCache sess yr = \case
    AoCPrompt d      -> Just $ printf "prompt/%04d/%02d.html"        yr (dayInt d)
    AoCInput  d      -> Just $ printf "input/%s%04d/%02d.txt" keyDir yr (dayInt d)
    AoCSubmit{}      -> Nothing
    AoCLeaderboard{} -> Nothing
    AoCDailyLeaderboard{} -> Nothing
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

    cacher . runExceptT $ do
      forM_ (aocDay a) $ \d -> do
        rel <- liftIO $ timeToRelease _aYear d
        when (rel > 0) $
          throwError $ AoCReleaseError rel

      mtr <- liftIO
           . throttling aocThrottler (max 1000000 _aThrottle)
           $ runClientM (aocReq _aYear a) =<< aocClientEnv _aSessionKey
      mcr <- maybe (throwError AoCThrottleError) pure mtr
      either (throwError . AoCClientError) pure mcr

aocClientEnv :: String -> IO ClientEnv
aocClientEnv s = do
    t <- getCurrentTime
    v <- atomically . newTVar $ createCookieJar [c t]
    mgr <- newTlsManager
    pure $ ClientEnv mgr aocBase (Just v)
  where
    c t = Cookie
      { cookie_name             = "session"
      , cookie_value            = T.encodeUtf8 . T.pack $ s
      , cookie_expiry_time      = addUTCTime oneYear t
      , cookie_domain           = "adventofcode.com"
      , cookie_path             = "/"
      , cookie_creation_time    = t
      , cookie_last_access_time = t
      , cookie_persistent       = True
      , cookie_host_only        = True
      , cookie_secure_only      = True
      , cookie_http_only        = True
      }
    oneYear = 60 * 60 * 24 * 356.25


saverLoader :: AoC a -> SaverLoader (Either AoCError a)
saverLoader = \case
    AoCPrompt d -> SL { _slSave = either (const Nothing) (Just . encodeMap)
                      , _slLoad = \str ->
                          let mp     = decodeMap str
                              hasAll = S.null (expectedParts d `S.difference` M.keysSet mp)
                          in  Right mp <$ guard hasAll
                      }
    AoCInput{}  -> SL { _slSave = either (const Nothing) Just
                      , _slLoad = Just . Right
                      }
    AoCSubmit{} -> noCache
    AoCLeaderboard{} -> noCache
    AoCDailyLeaderboard{} -> noCache
  where
    expectedParts :: Day -> Set Part
    expectedParts d
      | d == maxBound = S.singleton Part1
      | otherwise     = S.fromDistinctAscList [Part1 ..]
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

-- | Get time until release of a given challenge.
timeToRelease
    :: Integer              -- ^ year
    -> Day                  -- ^ day
    -> IO NominalDiffTime
timeToRelease y d = (challengeReleaseTime y d `diffUTCTime`) <$> getCurrentTime

-- | Check if a challenge has been released yet.
challengeReleased
    :: Integer              -- ^ year
    -> Day                  -- ^ day
    -> IO Bool
challengeReleased y = fmap (<= 0) . timeToRelease y

-- | Prompt release time
challengeReleaseTime
    :: Integer              -- ^ year
    -> Day                  -- ^ day
    -> UTCTime
challengeReleaseTime y d = UTCTime (fromGregorian y 12 (fromIntegral (dayInt d)))
                                   (5 * 60 * 60)

strip :: String -> String
strip = T.unpack . T.strip . T.pack
