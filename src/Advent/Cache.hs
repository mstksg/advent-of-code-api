{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}

module Advent.Cache (
    cacheing
  , SaverLoader(..)
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           System.Directory
import           System.FilePath
import           System.IO.Error

data SaverLoader a =
     SL { _slSave :: a -> Maybe String
        , _slLoad :: String -> Maybe a
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
        liftIO . mapM_ (writeFile fp) $ _slSave r
        pure r
      Just o  -> pure o

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe =
     (traverse (evaluate . forceString) . either (const Nothing) Just =<<)
   . tryJust (guard . isDoesNotExistError)
   . readFile

forceString :: String -> String
forceString xs = length xs `seq` xs
