{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

-- |
-- Module      : Advent.API
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Raw Servant API for Advent of Code.  Can be useful for building mock
-- servers, generating documentation and other servanty things, or
-- low-level raw requests.
--
-- If you use this to make requests directly, please use responsibly: do
-- not make automated requests more than once per day and throttle all
-- manual requestes.  See notes in "Advent".
--
-- @since 0.2.0.0
--

module Advent.API (
  -- * Types
    Day(..)
  , Part(..)
  , SubmitInfo(..)
  , SubmitRes(..), showSubmitRes
  , PublicCode(..)
  , Leaderboard(..)
  , LeaderboardMember(..)
  -- * Servant API
  , AdventAPI
  , adventAPI
  , adventAPIClient
  , adventAPIPuzzleClient
  -- * Util
  , mkDay, mkDay_, dayInt
  , partInt
  , partChar
  -- * Internal
  , processHTML
  , parseSubmitRes
  , Articles
  , FromArticles(..)
  , RawText
  ) where

-- import qualified Data.Attoparsec.Text    as P
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Proxy
import           Data.Text                  (Text)
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Data.Void
import           GHC.Generics
import           Servant.API
import           Servant.Client
import           Text.HTML.TagSoup.Tree     (TagTree(..))
import           Text.Printf
import           Text.Read                  (readMaybe)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Network.HTTP.Media         as M
import qualified Text.HTML.TagSoup          as H
import qualified Text.HTML.TagSoup.Tree     as H
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as P
import qualified Web.FormUrlEncoded         as WF

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
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
--
-- Note also that Challenge #25 typically only has a single part.
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

-- | Raw "text/plain" MIME type
data RawText

instance Accept RawText where
    contentType _ = "text" M.// "plain"

instance MimeUnrender RawText Text where
    mimeUnrender _ = first show . T.decodeUtf8' . BSL.toStrict

-- | Interpret repsonse as a list of HTML 'Text' in @<article>@ tags.
data Articles

-- | Class for interpreting a list of 'Text' in article tags to some
-- desired output.
class FromArticles a where
    fromArticles :: [Text] -> a

instance Accept Articles where
    contentType _ = "text" M.// "html"

instance FromArticles a => MimeUnrender Articles a where
    mimeUnrender _ = fmap fromArticles
                   . bimap show processHTML
                   . T.decodeUtf8'
                   . BSL.toStrict

instance FromArticles [Text] where
    fromArticles = id

instance FromArticles Text where
    fromArticles = T.unlines

instance (Ord a, Enum a, Bounded a) => FromArticles (Map a Text) where
    fromArticles = M.fromList . zip [minBound ..]

instance (FromArticles a, FromArticles b) => FromArticles (a :<|> b) where
    fromArticles xs = fromArticles xs :<|> fromArticles xs

instance FromArticles SubmitRes where
    fromArticles = parseSubmitRes . fold  . listToMaybe

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

instance FromJSONKey Day where
    fromJSONKey = FromJSONKeyTextParser (parseJSON . String)
instance FromJSONKey Part where
    fromJSONKey = FromJSONKeyTextParser (parseJSON . String)

instance FromJSON Part where
    parseJSON = withText "Part" $ \case
      "1" -> pure Part1
      "2" -> pure Part2
      _   -> fail "Bad part"
instance FromJSON Day where
    parseJSON = withText "Day" $ \t ->
      case readMaybe (T.unpack t) of
        Nothing -> fail "No read day"
        Just i  -> case mkDay i of
          Nothing -> fail "Day out of range"
          Just d  -> pure d



-- | REST API of Advent of Code.
--
-- Note that most of these requests assume a "session=" cookie.
type AdventAPI =
      Capture "year" Integer
   :> ("day" :> Capture "day" Day
             :> (Get '[Articles] (Map Part Text)
            :<|> "input" :> Get '[RawText] Text
            :<|> "answer"
                     :> ReqBody '[FormUrlEncoded] SubmitInfo
                     :> Post    '[Articles] (Text :<|> SubmitRes)
                )
  :<|> "leaderboard" :> "private" :> "view"
                     :> Capture "code" PublicCode
                     :> Get '[JSON] Leaderboard
      )

-- | 'Proxy' used for /servant/ functions.
adventAPI :: Proxy AdventAPI
adventAPI = Proxy

-- | 'ClientM' requests based on 'AdventAPI', generated by servant.
adventAPIClient
    :: Integer
    -> (Day -> ClientM (Map Part Text) :<|> ClientM Text :<|> (SubmitInfo -> ClientM (Text :<|> SubmitRes)) )
  :<|> (PublicCode -> ClientM Leaderboard)
adventAPIClient = client adventAPI

-- | A subset of 'adventAPIClient' for only puzzle-related API routes, not
-- leaderboard ones.
adventAPIPuzzleClient
    :: Integer
    -> Day
    -> ClientM (Map Part Text) :<|> ClientM Text :<|> (SubmitInfo -> ClientM (Text :<|> SubmitRes))
adventAPIPuzzleClient y = pis
  where
    pis :<|> _ = adventAPIClient y

-- | Process an HTML webpage into a list of all contents in <article>s
processHTML :: Text -> [Text]
processHTML = map H.renderTree
            . mapMaybe isArticle
            . H.universeTree
            . H.parseTree
  where
    isArticle :: TagTree Text -> Maybe [TagTree Text]
    isArticle (TagBranch n _ ts) = ts <$ guard (n == "article")
    isArticle _                  = Nothing

-- | Parse 'Text' into a 'SubmitRes'.
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

-- | A character associated with a given part.  'Part1' is associated with
-- @\'a\'@, and 'Part2' is associated with @\'b\'@
partChar :: Part -> Char
partChar Part1 = 'a'
partChar Part2 = 'b'

