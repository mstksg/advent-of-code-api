{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}

-- |
-- Module      : Advent.Throttle
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- (Internal) Implement cacheing of API requests.

module Advent.Cache (
    cacheing
  , SaverLoader(..)
  , noCache
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Text              (Text)
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified Data.Text.IO           as T

data SaverLoader a =
     SL { _slSave :: a -> Maybe Text
        , _slLoad :: Text -> Maybe a
        }

noCache :: SaverLoader a
noCache = SL (const Nothing) (const Nothing)

cacheing
    :: MonadIO m
    => FilePath
    -> SaverLoader a
    -> m a
    -> m a
cacheing fp SL{..} act = do
    old <- liftIO $ do
      createDirectoryIfMissing True (takeDirectory fp)
      (_slLoad =<<) <$> readFileMaybe fp
    case old of
      Nothing -> do
        r <- act
        liftIO . mapM_ (T.writeFile fp) $ _slSave r
        pure r
      Just o  -> pure o

readFileMaybe :: FilePath -> IO (Maybe Text)
readFileMaybe =
     (traverse (evaluate . force) . either (const Nothing) Just =<<)
   . tryJust (guard . isDoesNotExistError)
   . T.readFile
