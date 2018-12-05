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
import qualified Data.Yaml              as Y

data SaverLoader a = forall b. (Y.ToJSON b, Y.FromJSON b)
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
        liftIO . mapM_ (Y.encodeFile fp) $ _slSave r
        pure r
      Just o  -> pure o

readFileMaybe :: Y.FromJSON a => FilePath -> IO (Maybe a)
readFileMaybe =
     (traverse evaluate . either (const Nothing) Just . join . (fmap . first) (const ()) =<<)
   . tryJust (guard . isDoesNotExistError)
   . Y.decodeFileEither
