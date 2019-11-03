{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Advent.API (
    Day(..)
  , Part(..)
  , Session(..)
  , SubmitInfo(..)
  , AdventAPI
  , adventAPI
  , adventAPIClient
  ) where

import           Data.Aeson
import           Data.Finite
import           Data.Proxy
import           Data.Text      (Text)
import           Data.Typeable
import           GHC.Generics
import           Servant.API
import           Servant.Client
import           Text.Printf
import qualified Data.Text      as T

newtype Day = Day { getDay :: Finite 25 }

newtype Session = Session { getSession :: String }

data Part = Part1 | Part2
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

data SubmitInfo = SubmitInfo
    { siLevel  :: Part
    , siAnswer :: String
    }
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

instance ToHttpApiData Session where
    toUrlPiece   = T.pack . printf "session=%s" . getSession
    toQueryParam = toUrlPiece

instance ToHttpApiData Day where
    toUrlPiece = T.pack . show . dayInt
    toQueryParam = toUrlPiece

instance ToJSON Part where
    toJSON = Number . fromIntegral . partInt

instance ToJSON SubmitInfo where
    toEncoding = genericToEncoding defaultOptions
      { fieldLabelModifier = camelTo2 '-' . drop 2
      }

type AdventAPI = Header "Set-Cookie" Session
              :> Capture "year" Integer
              :> "day"
              :> Capture "day" Day
              :> (Get '[PlainText] Text
             :<|> "input" :> Get '[PlainText] Text
             :<|> "answer"
                      :> ReqBody '[JSON] SubmitInfo
                      :> Post    '[PlainText] Text
                 )

adventAPI :: Proxy AdventAPI
adventAPI = Proxy

adventAPIClient
    :: Maybe Session
    -> Integer
    -> Day
    -> ClientM Text :<|> ClientM Text :<|> (SubmitInfo -> ClientM Text)
adventAPIClient = client adventAPI

-- | Convert a @'Finite' 25@ day into a day integer (1 - 25).  Inverse of
-- 'mkDay'.
dayInt :: Day -> Integer
dayInt = (+ 1) . getFinite . getDay

-- | Convert a 'Part' to an 'Int'.
partInt :: Part -> Int
partInt Part1 = 1
partInt Part2 = 2
