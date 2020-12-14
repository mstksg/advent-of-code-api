{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- |
-- Module      : Advent.Types
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Data types used for the underlying API.
--
-- @since 0.2.3.0
--

module Advent.Types (
  -- * Types
    Day(..)
  , Part(..)
  , SubmitInfo(..)
  , SubmitRes(..), showSubmitRes
  , PublicCode(..)
  , Leaderboard(..)
  , LeaderboardMember(..)
  , Rank(..)
  , DailyLeaderboard(..)
  , DailyLeaderboardMember(..)
  , GlobalLeaderboard(..)
  , GlobalLeaderboardMember(..)
  , NextDayTime(..)
  -- * Util
  , mkDay, mkDay_, dayInt
  , _DayInt, pattern DayInt
  , partInt
  , partChar
  , fullDailyBoard
  , dlbmCompleteTime
  , dlbmTime
  , challengeReleaseTime
  -- * Internal
  , parseSubmitRes
  ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Functor.Classes
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Profunctor
import           Data.Text                  (Text)
import           Data.Time hiding           (Day)
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Data.Void
import           GHC.Generics
import           Servant.API
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Text.HTML.TagSoup          as H
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Web.FormUrlEncoded         as WF

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

#if !MIN_VERSION_time(1,9,0)
import           Data.Time.LocalTime.Compat
#endif

-- | Describes the day: a number between 1 and 25 inclusive.
--
-- Represented by a 'Finite' ranging from 0 to 24 inclusive; you should
-- probably make one using the smart constructor 'mkDay'.
newtype Day = Day { dayFinite :: Finite 25 }
  deriving (Eq, Ord, Enum, Bounded, Typeable, Generic)

instance Show Day where
    showsPrec = showsUnaryWith (\d -> showsPrec d . dayInt) "mkDay"

-- | A given part of a problem.  All Advent of Code challenges are
-- two-parts.
--
-- You can usually get 'Part1' (if it is already released) with a nonsense
-- session key, but 'Part2' always requires a valid session key.
data Part = Part1 | Part2
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

-- | Info required to submit an answer for a part.
data SubmitInfo = SubmitInfo
    { siLevel  :: Part
    , siAnswer :: String
    }
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

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

-- | Member ID of public leaderboard (the first part of the registration
-- code, before the hyphen).  It can be found as the number in the URL:
--
-- > https://adventofcode.com/2019/leaderboard/private/view/12345
--
-- (the @12345@ above)
newtype PublicCode = PublicCode { getPublicCode :: Integer }
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

-- | Leaderboard type, representing private leaderboard information.
data Leaderboard = LB
    { lbEvent   :: Integer                        -- ^ The year of the event
    , lbOwnerId :: Integer                        -- ^ The Member ID of the owner, or the public code
    , lbMembers :: Map Integer LeaderboardMember  -- ^ A map from member IDs to their leaderboard info
    }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Leaderboard position for a given member.
data LeaderboardMember = LBM
    { lbmGlobalScore :: Integer                     -- ^ Global leaderboard score
    , lbmName        :: Maybe Text                  -- ^ Username, if user specifies one
    , lbmLocalScore  :: Integer                     -- ^ Score for this leaderboard
    , lbmId          :: Integer                     -- ^ Member ID
    , lbmLastStarTS  :: Maybe UTCTime               -- ^ Time of last puzzle solved, if any
    , lbmStars       :: Int                         -- ^ Number of stars (puzzle parts) solved
    , lbmCompletion  :: Map Day (Map Part UTCTime)  -- ^ Completion times of each day and puzzle part
    }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Ranking between 1 to 100, for daily and global leaderboards
--
-- Note that 'getRank' interanlly stores a number from 0 to 99, so be sure
-- to add or subtract accordingly if you want to display or parse it.
--
-- @since 0.2.3.0
newtype Rank = Rank { getRank :: Finite 100 }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Single daily leaderboard position
--
-- @since 0.2.3.0
data DailyLeaderboardMember = DLBM
    { dlbmRank      :: Rank
    -- | Time from midnight EST of December 1st for that event.  Use
    -- 'dlbmCompleteTime' to convert to an actual time for event
    -- completion, and 'dlbmTime' to get the time it took to solve.
    --
    -- @since 0.2.7.0
    , dlbmDecTime   :: NominalDiffTime      -- ^ time from midnight EST.
    , dlbmUser      :: Either Integer Text
    , dlbmLink      :: Maybe Text
    , dlbmImage     :: Maybe Text
    , dlbmSupporter :: Bool
    }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Turn a 'dlbmDecTime' field into a 'ZonedTime' for the actual
-- completion of the puzzle, based on the year and day of event.
--
-- @since 0.2.7.0
dlbmCompleteTime :: Integer -> Day -> NominalDiffTime -> ZonedTime
dlbmCompleteTime y d t = r
    { zonedTimeToLocalTime = dlbmTime d t `addLocalTime` zonedTimeToLocalTime r
    }
  where
    r = challengeReleaseTime y d

-- | Turn a 'dlbmDecTime' field into a 'NominalDiffTime' representing the
-- actual amount of time taken to complete the puzzle.
--
-- @since 0.2.7.0
dlbmTime :: Day -> NominalDiffTime -> NominalDiffTime
dlbmTime d = uncurry daysAndTimeOfDayToTime
           . first (subtract (dayInt d - 1))
           . timeToDaysAndTimeOfDay

-- | Daily leaderboard, containing Star 1 and Star 2 completions
--
-- @since 0.2.3.0
data DailyLeaderboard = DLB {
    dlbStar1 :: Map Rank DailyLeaderboardMember
  , dlbStar2 :: Map Rank DailyLeaderboardMember
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Single global leaderboard position
--
-- @since 0.2.3.0
data GlobalLeaderboardMember = GLBM
    { glbmRank      :: Rank
    , glbmScore     :: Integer
    , glbmUser      :: Either Integer Text
    , glbmLink      :: Maybe Text
    , glbmImage     :: Maybe Text
    , glbmSupporter :: Bool
    }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Global leaderboard for the entire event
--
-- Under each 'Rank' is an 'Integer' for the score at that rank, as well as
-- a non-empty list of all members who achieved that rank and score.
--
-- @since 0.2.3.0
newtype GlobalLeaderboard = GLB {
    glbMap :: Map Rank (Integer, NonEmpty GlobalLeaderboardMember)
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | The next day for a challenge in a given year, and also the number of
-- seconds until the challenge is released.
--
-- @since 0.2.8.0
data NextDayTime = NextDayTime Day Int
                 | NoNextDayTime
  deriving (Show, Eq, Ord, Typeable, Generic)

instance ToHttpApiData Part where
    toUrlPiece = T.pack . show . partInt
    toQueryParam = toUrlPiece

instance ToHttpApiData Day where
    toUrlPiece = T.pack . show . dayInt
    toQueryParam = toUrlPiece

instance ToHttpApiData PublicCode where
    toUrlPiece   = (<> ".json") . T.pack . show . getPublicCode
    toQueryParam = toUrlPiece

instance WF.ToForm SubmitInfo where
    toForm = WF.genericToForm WF.FormOptions
      { WF.fieldLabelModifier = camelTo2 '-' . drop 2 }

instance FromJSON Leaderboard where
    parseJSON = withObject "Leaderboard" $ \o ->
        LB <$> (strInt =<< (o .: "event"))
           <*> (strInt =<< (o .: "owner_id"))
           <*> o .: "members"
      where
        strInt t = case readMaybe t of
          Nothing -> fail "bad int"
          Just i  -> pure i

instance FromJSON LeaderboardMember where
    parseJSON = withObject "LeaderboardMember" $ \o ->
        LBM <$> o .: "global_score"
            <*> optional (o .: "name")
            <*> o .: "local_score"
            <*> (strInt =<< (o .: "id"))
            <*> optional (fromEpoch =<< (o .: "last_star_ts"))
            <*> o .: "stars"
            <*> (do cdl <- o .: "completion_day_level"
                    (traverse . traverse) ((fromEpoch =<<) . (.: "get_star_ts")) cdl
                )
      where
        strInt t = case readMaybe t of
          Nothing -> fail "bad int"
          Just i  -> pure i
        fromEpoch t = case readMaybe t of
          Nothing -> fail "bad stamp"
          Just i  -> pure . posixSecondsToUTCTime $ fromInteger i

-- | @since 0.2.4.2
instance ToJSONKey Day where
    toJSONKey = toJSONKeyText $ T.pack . show . dayInt
instance FromJSONKey Day where
    fromJSONKey = FromJSONKeyTextParser (parseJSON . String)
-- | @since 0.2.4.2
instance ToJSONKey Part where
    toJSONKey = toJSONKeyText $ \case
      Part1 -> "1"
      Part2 -> "2"
instance FromJSONKey Part where
    fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

-- | @since 0.2.4.2
instance ToJSON Part where
    toJSON = String . (\case Part1 -> "1"; Part2 -> "2")
instance FromJSON Part where
    parseJSON = withText "Part" $ \case
      "1" -> pure Part1
      "2" -> pure Part2
      _   -> fail "Bad part"

-- | @since 0.2.4.2
instance ToJSON Day where
    toJSON = String . T.pack . show . dayInt
instance FromJSON Day where
    parseJSON = withText "Day" $ \t ->
      case readMaybe (T.unpack t) of
        Nothing -> fail "No read day"
        Just i  -> case mkDay i of
          Nothing -> fail "Day out of range"
          Just d  -> pure d

instance ToJSONKey Rank where
    toJSONKey = toJSONKeyText $ T.pack . show . (+ 1) . getFinite . getRank
instance FromJSONKey Rank where
    fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance ToJSON Rank where
    toJSON = String . T.pack . show . (+ 1) . getFinite . getRank
instance FromJSON Rank where
    parseJSON = withText "Rank" $ \t ->
      case readMaybe (T.unpack t) of
        Nothing -> fail "No read rank"
        Just i  -> case packFinite (i - 1) of
          Nothing -> fail "Rank out of range"
          Just d  -> pure $ Rank d

instance ToJSON DailyLeaderboard where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 3 }
instance FromJSON DailyLeaderboard where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 3 }

instance ToJSON GlobalLeaderboard where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 3 }
instance FromJSON GlobalLeaderboard where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 3 }

instance ToJSON DailyLeaderboardMember where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 4 }
instance FromJSON DailyLeaderboardMember where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 4 }

instance ToJSON GlobalLeaderboardMember where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 4 }
instance FromJSON GlobalLeaderboardMember where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = camelTo2 '-' . drop 4 }

-- | Parse 'T.Text' into a 'SubmitRes'.
parseSubmitRes :: Text -> SubmitRes
parseSubmitRes = either (SubUnknown . P.errorBundlePretty) id
               . P.runParser choices "Submission Response"
               . mconcat
               . mapMaybe deTag
               . H.parseTags
  where
    deTag (H.TagText t) = Just t
    deTag _             = Nothing
    choices             = asum [ P.try parseCorrect   P.<?> "Correct"
                               , P.try parseIncorrect P.<?> "Incorrect"
                               , P.try parseWait      P.<?> "Wait"
                               ,       parseInvalid   P.<?> "Invalid"
                               ]
    parseCorrect :: P.Parsec Void Text SubmitRes
    parseCorrect = do
      _ <- P.manyTill P.anySingle (P.string' "that's the right answer") P.<?> "Right answer"
      r <- optional . (P.<?> "Rank") . P.try $ do
        P.manyTill P.anySingle (P.string' "rank")
          *> P.skipMany (P.satisfy (not . isDigit))
        P.decimal
      pure $ SubCorrect r
    parseIncorrect = do
      _ <- P.manyTill P.anySingle (P.string' "that's not the right answer") P.<?> "Not the right answer"
      hint <- optional . (P.<?> "Hint") . P.try $ do
        P.manyTill P.anySingle "your answer is" *> P.space1
        P.takeWhile1P (Just "dot") (/= '.')
      P.manyTill P.anySingle (P.string' "wait") *> P.space1
      waitAmt <- (1 <$ P.string' "one") <|> P.decimal
      pure $ SubIncorrect (waitAmt * 60) (T.unpack <$> hint)
    parseWait = do
      _ <- P.manyTill P.anySingle (P.string' "an answer too recently") P.<?> "An answer too recently"
      P.skipMany (P.satisfy (not . isDigit))
      m <- optional . (P.<?> "Delay minutes") . P.try $
              P.decimal <* P.char 'm' <* P.space1
      s <- P.decimal <* P.char 's' P.<?> "Delay seconds"
      pure . SubWait $ maybe 0 (* 60) m + s
    parseInvalid = SubInvalid <$ P.manyTill P.anySingle (P.string' "solving the right level")

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

-- | Convert a @'Finite' 25@ day into a day integer (1 - 25).  Inverse of
-- 'mkDay'.
dayInt :: Day -> Integer
dayInt = (+ 1) . getFinite . dayFinite

-- | Convert a 'Part' to an 'Int'.
partInt :: Part -> Int
partInt Part1 = 1
partInt Part2 = 2

-- | Construct a 'Day' from a day integer (1 - 25).  If input is out of
-- range, 'Nothing' is returned.  See 'mkDay_' for an unsafe version useful
-- for literals.
--
-- Inverse of 'dayInt'.
mkDay :: Integer -> Maybe Day
mkDay = fmap Day . packFinite . subtract 1

-- | Construct a @'Finite' 25@ (the type of a Day) from a day
-- integer (1 - 25).  Is undefined if input is out of range.  Can be useful
-- for compile-time literals, like @'mkDay_' 4@
--
-- Inverse of 'dayInt'.
mkDay_ :: Integer -> Day
mkDay_ = fromMaybe e . mkDay
  where
    e = errorWithoutStackTrace "Advent.mkDay_: Date out of range (1 - 25)"

-- | This is a @Prism' 'Integer' 'Day'@ , to treat an 'Integer' as if it
-- were a 'Day'.
--
-- @since 0.2.4.0
_DayInt :: (Choice p, Applicative f) => p Day (f Day) -> p Integer (f Integer)
_DayInt = dimap a b . right'
  where
    a i = maybe (Left i) Right . mkDay $ i
    b   = either pure (fmap dayInt)

-- | Pattern synonym allowing you to match on an 'Integer' as if it were
-- a 'Day':
--
-- @
-- case myInt of
--   DayInt d -> ...
--   _        -> ...
-- @
--
-- Will fail if the integer is out of bounds (outside of 1-25)
--
-- @since 0.2.4.0
pattern DayInt :: Day -> Integer
pattern DayInt d <- (mkDay->Just d)
  where
    DayInt d = dayInt d

-- | A character associated with a given part.  'Part1' is associated with
-- @\'a\'@, and 'Part2' is associated with @\'b\'@
partChar :: Part -> Char
partChar Part1 = 'a'
partChar Part2 = 'b'

-- | Check if a 'DailyLeaderboard' is filled up or not.
--
-- @since 0.2.4.0
fullDailyBoard
    :: DailyLeaderboard
    -> Bool
fullDailyBoard DLB{..} = (M.size dlbStar1 + M.size dlbStar2) >= 200

-- | Prompt release time.
--
-- Changed from 'UTCTime' to 'ZonedTime' in v0.2.7.0.  To use as
-- a 'UTCTime', use 'zonedTimeToUTC'.
challengeReleaseTime
    :: Integer              -- ^ year
    -> Day                  -- ^ day
    -> ZonedTime
challengeReleaseTime y d = ZonedTime
    { zonedTimeToLocalTime = LocalTime
        { localDay       = fromGregorian y 12 (fromIntegral (dayInt d))
        , localTimeOfDay = midnight
        }
    , zonedTimeZone = read "EST"
    }

