{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Advent.API (
    Day(..)
  , Part(..)
  , SubmitInfo(..)
  , AdventAPI
  , adventAPI
  , adventAPIClient
  , mkDay, mkDay_, dayInt
  , partInt
  , partChar
  ) where

import           Data.Aeson
import           Data.Bifunctor
import           Data.Finite
import           Data.Functor.Classes
import           Data.Maybe
import           Data.Proxy
import           Data.Text            (Text)
import           Data.Typeable
import           GHC.Generics
import           Servant.API
import           Servant.Client
import           Text.Printf
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Media   as M

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

data SubmitInfo = SubmitInfo
    { siLevel  :: Part
    , siAnswer :: String
    }
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance ToHttpApiData Day where
    toUrlPiece = T.pack . show . dayInt
    toQueryParam = toUrlPiece

instance ToJSON Part where
    toJSON = Number . fromIntegral . partInt

instance ToJSON SubmitInfo where
    toEncoding = genericToEncoding defaultOptions
      { fieldLabelModifier = camelTo2 '-' . drop 2
      }

data RawText

instance Accept RawText where
    contentType _ = "text" M.// "plain"

instance MimeUnrender RawText Text where
    mimeUnrender _ = first show . T.decodeUtf8' . BSL.toStrict

type AdventAPI = Capture "year" Integer
              :> "day"
              :> Capture "day" Day
              :> (Get '[PlainText] Text
             :<|> "input" :> Get '[RawText] Text
             :<|> "answer"
                      :> ReqBody '[JSON] SubmitInfo
                      :> Post    '[PlainText] Text
                 )

adventAPI :: Proxy AdventAPI
adventAPI = Proxy

adventAPIClient
    :: Integer
    -> Day
    -> ClientM Text :<|> ClientM Text :<|> (SubmitInfo -> ClientM Text)
adventAPIClient = client adventAPI

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

