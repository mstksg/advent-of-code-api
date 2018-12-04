{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeInType        #-}

module Advent (
  ) where

import           Control.Monad.Except
import           Data.Char
import           Data.Finite
import           Data.Kind
import           Data.Map             (Map)
import           Data.Maybe
import           Data.Semigroup
import           Data.Text            (Text)
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

-- | An API command.  An @'AoC' k a@ an AoC API request that returns
-- results of type @a@; if @k@ is ''True', it requires a session key.  If
-- @k@ is ''False', it does not.
data AoC :: Bool -> Type -> Type where
    -- | Fetch prompts, as Markdown.
    AoCPrompt
        :: Finite 25                    -- ^ Day.
        -> AoC k      (Map Char Text)   -- ^ Map of prompts (as markdown). Part 1 is under @\'a\'@,
                                        --   Part 2 is under @\'b\'@, etc.
    -- | Fetch input
    AoCInput
        :: Finite 25                    -- ^ Day.
        -> AoC 'True Text
    -- | Submit answer.
    AoCSubmit
        :: Finite 25                    -- ^ Day.
        -> Char                         -- ^ Part.  \'a\' for part 1, \'b\' for part 2, etc.
        -> String                       -- ^ Answer.  __WARNING__: not escaped or length-limited.
        -> AoC 'True (Text, SubmitRes)  -- ^ Submission reply (as markdown), and result token

data AoCError =
    -- | A libcurl error, with response code and response body
    ACurlError CurlCode String

data SessionKey :: Bool -> Type where
    HasKey :: String -> SessionKey k
    NoKey  :: SessionKey 'False

data AoCSettings k = AoCSettings { _aSessionKey :: SessionKey k
                             , _aYear       :: Integer
                             , _aCache      :: FilePath
                             , _aThrottle   :: Int
                             }

apiUrl :: Integer -> AoC k a -> FilePath
apiUrl y = \case
    AoCPrompt i     -> printf "https://adventofcode.com/%04y/day/%d"        y (dNum i)
    AoCInput  i     -> printf "https://adventofcode.com/%04y/day/%d/input"  y (dNum i)
    AoCSubmit i _ _ -> printf "https://adventofcode.com/%04y/day/%d/answer" y (dNum i)
  where
    dNum = (+ 1) . getFinite

sessionKeyCookie :: SessionKey 'True -> CurlOption
sessionKeyCookie (HasKey s) = CurlCookie $ printf "session=%s" s

sessionKeyCookieMaybe :: SessionKey k -> Maybe CurlOption
sessionKeyCookieMaybe (HasKey s) = Just (sessionKeyCookie (HasKey s))
sessionKeyCookieMaybe NoKey      = Nothing

-- | WARNING: does not escape submission answers or limit their length.
apiCurl :: SessionKey k -> AoC k a -> [CurlOption]
apiCurl sess = \case
    AoCPrompt _       -> maybeToList (sessionKeyCookieMaybe sess)
                      ++ method_GET
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

-- | Run an 'API' command with a given 'SessionKey' to produce the result
-- or a list of (lines of) errors.
--
-- __WARNING__: Does not escape submission answers or limit their length,
-- for 'ASubmit'.
runAoC :: AoCSettings k -> AoC k a -> IO (Either AoCError a)
runAoC AoCSettings{..} a = withCurlDo . runExceptT $ do
    (cc, r) <- liftIO $ curlGetString u (apiCurl _aSessionKey a)
    case cc of
      CurlOK -> return ()
      _      -> throwError $ ACurlError cc r
    pure $ processAoC a r
  where
    u = apiUrl _aYear a

processAoC :: AoC k a -> String -> a
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

universe :: H.Node -> [H.Node]
universe = ($ []) . appEndo . go
  where
    go :: H.Node -> Endo [H.Node]
    go (H.NodeElement (H.Element{..})) = Endo (eltChildren ++)
                                      <> foldMap go eltChildren
    go (H.NodeContent _              ) = mempty

strip :: String -> String
strip = T.unpack . T.strip . T.pack
