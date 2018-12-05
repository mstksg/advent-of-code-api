{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType         #-}

module Advent (
  -- * API
    AoC(..)
  , AoCSettings(..)
  , SubmitRes(..), showSubmitRes
  , runAoC
  ) where

import           Control.Exception
import           Control.Monad.Except
import           Data.Char
import           Data.Finite
import           Data.Kind
import           Data.Map             (Map)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text            (Text)
import           Data.Typeable
import           Network.Curl
import           Text.Printf
import           Text.Read            (readMaybe)
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Text.Taggy           as H

-- | The result of a submission.
data SubmitRes = SubCorrect (Maybe Integer)     -- ^ global rank
               | SubIncorrect
               | SubWait
               | SubInvalid
               | SubUnknown
  deriving (Show, Eq, Ord)

-- | An API command.  An @'AoC' a@ an AoC API request that returns
-- results of type @a@.
data AoC :: Type -> Type where
    -- | Fetch prompts, as HTML.
    AoCPrompt
        :: Finite 25             -- ^ Day.
        -> AoC (Map Char Text)   -- ^ Map of prompts (as markdown). Part 1 is under @\'a\'@,
                                 --   Part 2 is under @\'b\'@, etc.

    -- | Fetch input
    AoCInput
        :: Finite 25             -- ^ Day.
        -> AoC Text

    -- | Submit answer.
    AoCSubmit
        :: Finite 25              -- ^ Day.
        -> Char                   -- ^ Part.  \'a\' for part 1, \'b\' for part 2, etc.
        -> String                 -- ^ Answer.  __WARNING__: not escaped or length-limited.
        -> AoC (Text, SubmitRes)  -- ^ Submission reply (as HTML), and result token

-- | A possible (syncronous, logical, pure) error returnable from 'runAoC'.
-- Does not cover any asynchronous or IO errors.
data AoCError =
    -- | A libcurl error, with response code and response body
    ACurlError CurlCode String
  deriving (Show, Typeable)
instance Exception AoCError

-- | Setings for running an API request.
data AoCSettings = AoCSettings
    { _aSessionKey :: String
    , _aYear       :: Integer
    , _aCache      :: FilePath
    , _aThrottle   :: Int      -- ^ Throttle delay, in milliseconds.  Minimum is 1000000.
    }
  deriving Show

apiUrl :: Integer -> AoC a -> FilePath
apiUrl y = \case
    AoCPrompt i     -> printf "https://adventofcode.com/%04y/day/%d"        y (dNum i)
    AoCInput  i     -> printf "https://adventofcode.com/%04y/day/%d/input"  y (dNum i)
    AoCSubmit i _ _ -> printf "https://adventofcode.com/%04y/day/%d/answer" y (dNum i)
  where
    dNum = (+ 1) . getFinite

sessionKeyCookie :: String -> CurlOption
sessionKeyCookie = CurlCookie . printf "session=%s"

-- | WARNING: does not escape submission answers or limit their length.
apiCurl :: String -> AoC a -> [CurlOption]
apiCurl sess = \case
    AoCPrompt _       -> sessionKeyCookie sess
                       : method_GET
    AoCInput  _       -> sessionKeyCookie sess
                       : method_GET
    AoCSubmit _ p ans -> sessionKeyCookie sess
                       : CurlPostFields [ printf "level=%d" (partNum p)
                                        , printf "answer=%s" (strip ans)
                                        ]
                       : CurlHttpHeaders ["Content-Type: application/x-www-form-urlencoded"]
                       : method_POST
  where
    partNum p = ord p - ord 'a' + 1

-- | Run an 'API' command with a given 'AoCSettings' to produce the result
-- or a list of (lines of) errors.
--
-- __WARNING__: Does not escape submission answers or limit their length,
-- for 'ASubmit'.
runAoC :: AoCSettings -> AoC a -> IO (Either AoCError a)
runAoC AoCSettings{..} a = withCurlDo . runExceptT $ do
    (cc, r) <- liftIO $ curlGetString u (apiCurl _aSessionKey a)
    case cc of
      CurlOK -> return ()
      _      -> throwError $ ACurlError cc r
    pure $ processAoC a r
  where
    u = apiUrl _aYear a

processAoC :: AoC a -> String -> a
processAoC = \case
    AoCPrompt{} -> M.fromList
                 . zip ['a'..]
                 . processHTML
    AoCInput{}  -> T.pack
    AoCSubmit{} -> (\o -> (o, parseSubmitRes o))
                 . fromMaybe ""
                 . listToMaybe
                 . processHTML

-- | Process an HTML webpage into a list of all contents in <article>s
processHTML :: String -> [T.Text]
processHTML = map (TL.toStrict . TL.unlines . map H.render)
            . mapMaybe isArticle
            . foldMap universe
            . listToMaybe
            . H.parseDOM True
            . TL.pack
  where
    isArticle (H.NodeElement (H.Element{..}))
        = eltChildren <$ guard (eltName == "article")
    isArticle _
        = Nothing

parseSubmitRes :: T.Text -> SubmitRes
parseSubmitRes t
    | "the right answer!"       `T.isInfixOf` t = SubCorrect $ findRank t
    | "not the right answer."   `T.isInfixOf` t = SubIncorrect
    | "an answer too recently"  `T.isInfixOf` t = SubWait
    | "solving the right level" `T.isInfixOf` t = SubInvalid
    | otherwise                                 = SubUnknown
  where
    findRank = go . T.words . T.map onlyAlphaNum . T.toLower
      where
        go ("rank":n:_) = readMaybe $ T.unpack n
        go (_     :ws ) = go ws
        go []           = Nothing
        onlyAlphaNum c
          | isAlphaNum c = c
          | otherwise    = ' '

-- | Pretty-print a 'SubmitRes'
showSubmitRes :: SubmitRes -> String
showSubmitRes = \case
    SubCorrect Nothing  -> "Correct"
    SubCorrect (Just r) -> printf "Correct (Rank %d)" r
    SubIncorrect        -> "Incorrect"
    SubWait             -> "Wait"
    SubInvalid          -> "Invalid"
    SubUnknown          -> "Unknown"

universe :: H.Node -> [H.Node]
universe = ($ []) . appEndo . go
  where
    go :: H.Node -> Endo [H.Node]
    go (H.NodeElement (H.Element{..})) = Endo (eltChildren ++)
                                      <> foldMap go eltChildren
    go (H.NodeContent _              ) = mempty

strip :: String -> String
strip = T.unpack . T.strip . T.pack
