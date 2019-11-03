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
-- @since 0.2.0.0
--

module Advent.API (
  -- * Types
    Day(..)
  , Part(..)
  , SubmitInfo(..)
  , SubmitRes(..), showSubmitRes
  -- * Servant API
  , AdventAPI
  , adventAPI
  , adventAPIClient
  -- * Util
  , mkDay, mkDay_, dayInt
  , partInt
  , partChar
  -- * Internal
  , processHTML
  , parseSubmitRes
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Map               (Map)
import           Data.Maybe
import           Data.Proxy
import           Data.Text              (Text)
import           Data.Typeable
import           GHC.Generics
import           Servant.API
import           Servant.Client
import           Text.HTML.TagSoup.Tree (TagTree(..))
import           Text.Printf
import qualified Data.Attoparsec.Text   as P
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Network.HTTP.Media     as M
import qualified Text.HTML.TagSoup      as H
import qualified Text.HTML.TagSoup.Tree as H
import qualified Web.FormUrlEncoded     as WF

-- | Describes the day: a number between 1 and 25 inclusive.
--
-- Represented by a 'Finite' ranging from 0 to 24 inclusive; you should
-- probably make one using the smart constructor 'mkDay'.
newtype Day = Day { dayFinite :: Finite 25 }
  deriving (Eq, Ord, Enum, Bounded)

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

instance ToHttpApiData Part where
    toUrlPiece = T.pack . show . partInt
    toQueryParam = toUrlPiece

instance ToHttpApiData Day where
    toUrlPiece = T.pack . show . dayInt
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

-- | REST API of Advent of Code.
--
-- Note that most of these requests assume a "session=" cookie.
type AdventAPI = Capture "year" Integer
              :> "day"
              :> Capture "day" Day
              :> (Get '[Articles] (Map Part Text)
             :<|> "input" :> Get '[RawText] Text
             :<|> "answer"
                      :> ReqBody '[FormUrlEncoded] SubmitInfo
                      :> Post    '[Articles] (Text :<|> SubmitRes)
                 )

-- | 'Proxy' used for /servant/ functions.
adventAPI :: Proxy AdventAPI
adventAPI = Proxy

-- | 'ClientM' requests based on 'AdventAPI', generated by servant.
adventAPIClient
    :: Integer
    -> Day
    -> ClientM (Map Part Text) :<|> ClientM Text :<|> (SubmitInfo -> ClientM (Text :<|> SubmitRes))
adventAPIClient = client adventAPI

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
                               , fail "No option recognized"
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

