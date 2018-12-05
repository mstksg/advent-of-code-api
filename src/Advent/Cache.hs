{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}

module Advent.Cache (
    cacheing
  , SaverLoader(..)
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           System.Directory
import           System.FilePath
import           System.IO.Error
import qualified Data.Binary            as Bi
import qualified Data.Text              as T

data SaverLoader a = forall b. Bi.Binary b
  => SL { _slSave :: a -> Maybe b
        , _slLoad :: b -> Maybe a
        }

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
        liftIO . mapM_ (Bi.encodeFile fp) $ _slSave r
        pure r
      Just o  -> pure o

readFileMaybe :: Bi.Binary a => FilePath -> IO (Maybe a)
readFileMaybe =
     (traverse evaluate . either (const Nothing) Just . join . (fmap . first) (const ()) =<<)
   . tryJust (guard . isDoesNotExistError)
   . Bi.decodeFileOrFail

