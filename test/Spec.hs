{-# LANGUAGE ViewPatterns #-}

import           Advent
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.FilePath
import           Test.HUnit
import           Text.Read        (readMaybe)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T

fileTest :: FilePath -> IO Test
fileTest fp = do
    ls <- T.lines <$> T.readFile ("test-data" </> fp)
    (T.strip->x, T.unlines->xs) <- maybe (fail "Empty test file") pure $
                                      uncons ls

    r <- maybe (fail "No parse expected result") pure $
            readMaybe (T.unpack x)

    pure . TestLabel fp $ r ~=? parseSubmitRes xs

main :: IO ()
main = do
    tests <- fmap TestList
           . mapM fileTest
         =<< listDirectory "test-data"
    c <- runTestTT tests
    unless (failures c == 0) $
      exitFailure
