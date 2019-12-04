{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

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
-- If you use this to make requests directly, please use responsibly: do
-- not make automated requests more than once per day and throttle all
-- manual requestes.  See notes in "Advent".
--
-- @since 0.2.0.0
--

module Advent.API (
  -- * Servant API
    AdventAPI
  , adventAPI
  , adventAPIClient
  , adventAPIPuzzleClient
  -- * Types
  , HTMLTags
  , FromTags(..)
  , Articles
  , Divs
  , RawText
  -- * Internal
  , processHTML
  ) where

-- import           Data.Time.Clock
-- import           Data.Time.Format
-- import           Data.Time.LocalTime
import           Advent.Types
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char
import           Data.Finite
import           Data.Foldable
import           Data.List.NonEmpty     (NonEmpty(..))
import           Data.Map               (Map)
import           Data.Maybe
import           Data.Ord
import           Data.Proxy
import           Data.Text              (Text)
import           Data.Time hiding       (Day)
import           GHC.TypeLits
import           Servant.API
import           Servant.Client
import           Text.HTML.TagSoup.Tree (TagTree(..))
import           Text.Read              (readMaybe)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.List.NonEmpty     as NE
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Network.HTTP.Media     as M
import qualified Text.HTML.TagSoup      as H
import qualified Text.HTML.TagSoup.Tree as H

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif

-- | Raw "text/plain" MIME type
data RawText

instance Accept RawText where
    contentType _ = "text" M.// "plain"

instance MimeUnrender RawText Text where
    mimeUnrender _ = first show . T.decodeUtf8' . BSL.toStrict

-- | Interpret repsonse as a list of HTML 'T.Text' found in the given type of
-- tag
--
-- @since 0.2.3.0
data HTMLTags (tag :: Symbol)

-- | Interpret a response as a list of HTML 'T.Text' found in @<article>@ tags.
type Articles = HTMLTags "article"

-- | Interpret a response as a list of HTML 'T.Text' found in @<div>@ tags.
--
-- @since 0.2.3.0
type Divs     = HTMLTags "div"

-- | Class for interpreting a list of 'T.Text' in tags to some desired
-- output.
--
-- @since 0.2.3.0
class FromTags tag a where
    fromTags :: p tag -> [Text] -> Maybe a

instance Accept (HTMLTags cls) where
    contentType _ = "text" M.// "html"

instance (FromTags tag a, KnownSymbol tag) => MimeUnrender (HTMLTags tag) a where
    mimeUnrender _ str = do
      x <- first show . T.decodeUtf8' . BSL.toStrict $ str
      maybe (Left "No parse") pure
         . fromTags (Proxy @tag)
         . processHTML (symbolVal (Proxy @tag))
         $ x

instance FromTags cls [Text] where
    fromTags _ = Just

instance FromTags cls Text where
    fromTags _ = Just . T.unlines

instance (Ord a, Enum a, Bounded a) => FromTags cls (Map a Text) where
    fromTags _ = Just . M.fromList . zip [minBound ..]

instance (FromTags cls a, FromTags cls b) => FromTags cls (a :<|> b) where
    fromTags p xs = (:<|>) <$> fromTags p xs <*> fromTags p xs

instance FromTags "article" SubmitRes where
    fromTags _ = Just . parseSubmitRes . fold  . listToMaybe

instance FromTags "div" DailyLeaderboard where
    fromTags _ = Just . assembleDLB . mapMaybe parseMember
      where
        parseMember :: Text -> Maybe DailyLeaderboardMember
        parseMember contents = do
            dlbmRank <- fmap Rank . packFinite . subtract 1
                    =<< readMaybe . filter isDigit . T.unpack . fst
                    =<< findTag uni "span" (Just "leaderboard-position")
            dlbmDecTime <- fmap mkDiff
                         . parseTimeM True defaultTimeLocale "%b %d  %H:%M:%S"
                         . T.unpack . fst
                       =<< findTag uni "span" (Just "leaderboard-time")
            dlbmUser <- eitherUser tr
            pure DLBM{..}
          where
            dlbmLink      = lookup "href" . snd =<< findTag uni "a" Nothing
            dlbmSupporter = "AoC++" `T.isInfixOf` contents
            dlbmImage     = lookup "src" . snd =<< findTag uni "img" Nothing
            tr  = H.parseTree contents
            uni = H.universeTree tr
        assembleDLB = flipper . snd . foldl' (uncurry go) (Nothing, DLB M.empty M.empty)
          where
            flipper dlb@(DLB a b)
              | M.null a  = DLB b a
              | otherwise = dlb
            go counter dlb m@DLBM{..} = case counter of
                Nothing      -> dlb2
                Just Nothing -> dlb1
                Just (Just i)
                  | dlbmRank <= i -> dlb1
                  | otherwise     -> dlb2
              where
                dlb1 = (Just Nothing        , dlb { dlbStar1 = M.insert dlbmRank m (dlbStar1 dlb) })
                dlb2 = (Just (Just dlbmRank), dlb { dlbStar2 = M.insert dlbmRank m (dlbStar2 dlb) })
        mkDiff t = t `dlt` decemberFirst
          where
            -- diffLocalTime, but is only in time >= 1.9
            dlt a b = diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)
        decemberFirst = LocalTime (fromGregorian 1970 12 1) midnight

instance FromTags "div" GlobalLeaderboard where
    fromTags _ = Just . GLB . reScore . M.fromListWith (<>)
               . map (\x -> (Down (glbmScore x), x :| []))
               . mapMaybe parseMember
      where
        parseMember :: Text -> Maybe GlobalLeaderboardMember
        parseMember contents = do
            glbmScore <- readMaybe . filter isDigit . T.unpack . fst
                     =<< findTag uni "span" (Just "leaderboard-totalscore")
            glbmUser <- eitherUser tr
            pure GLBM{..}
          where
            glbmRank      = Rank 0
            glbmLink      = lookup "href" . snd =<< findTag uni "a" Nothing
            glbmSupporter = "AoC++" `T.isInfixOf` contents
            glbmImage     = lookup "src" . snd =<< findTag uni "img" Nothing
            tr  = H.parseTree contents
            uni = H.universeTree tr
        reScore = fmap (\xs -> (glbmScore (NE.head xs), xs))
                . M.fromList
                . flip evalState 0
                . traverse go
                . toList
          where
            go xs = do
              currScore <- get
              xs' <- forM xs $ \x -> x { glbmRank = Rank currScore } <$ modify succ
              pure (Rank currScore, xs')



-- | REST API of Advent of Code.
--
-- Note that most of these requests assume a "session=" cookie.
type AdventAPI =
      Capture "year" Integer
   :> ("day" :> Capture "day" Day
             :> (Get '[Articles] (Map Part Text)
            :<|> "input" :> Get '[RawText] Text
            :<|> "answer"
                     :> ReqBody '[FormUrlEncoded] SubmitInfo
                     :> Post    '[Articles] (Text :<|> SubmitRes)
                )
  :<|> "leaderboard"
    :> (Get '[Divs] GlobalLeaderboard
   :<|> "day"     :> Capture "day" Day :> Get '[Divs] DailyLeaderboard
   :<|> "private" :> "view"
                  :> Capture "code" PublicCode
                  :> Get '[JSON] Leaderboard

       )
      )

-- | 'Proxy' used for /servant/ functions.
adventAPI :: Proxy AdventAPI
adventAPI = Proxy

-- | 'ClientM' requests based on 'AdventAPI', generated by servant.
adventAPIClient
    :: Integer
    -> (Day -> ClientM (Map Part Text) :<|> ClientM Text :<|> (SubmitInfo -> ClientM (Text :<|> SubmitRes)) )
  :<|> ClientM GlobalLeaderboard
  :<|> (Day -> ClientM DailyLeaderboard)
  :<|> (PublicCode -> ClientM Leaderboard)
adventAPIClient = client adventAPI

-- | A subset of 'adventAPIClient' for only puzzle-related API routes, not
-- leaderboard ones.
adventAPIPuzzleClient
    :: Integer
    -> Day
    -> ClientM (Map Part Text) :<|> ClientM Text :<|> (SubmitInfo -> ClientM (Text :<|> SubmitRes))
adventAPIPuzzleClient y = pis
  where
    pis :<|> _ = adventAPIClient y

userNameNaked :: [TagTree Text] -> Maybe Text
userNameNaked = (listToMaybe .) . mapMaybe $ \x -> do
  TagLeaf (H.TagText (T.strip->u)) <- Just x
  guard . not $ T.null u
  pure u
findTag :: [TagTree Text] -> Text -> Maybe Text -> Maybe (Text, [H.Attribute Text])
findTag uni tag cls = listToMaybe . flip mapMaybe uni $ \x -> do
  TagBranch tag' attr cld <- Just x
  guard $ tag' == tag
  forM_ cls $ \c -> guard $ ("class", c) `elem` attr
  pure (H.renderTree cld, attr)
eitherUser :: [TagTree Text] -> Maybe (Either Integer Text)
eitherUser tr = asum [
      Right <$> userNameNaked tr
    , fmap Right $ userNameNaked . H.parseTree . fst
               =<< findTag uni "a" Nothing
    , fmap Left  $ readMaybe . filter isDigit . T.unpack . fst
               =<< findTag uni "span" (Just "leaderboard-anon")
    ]
  where
    uni = H.universeTree tr

-- | Process an HTML webpage into a list of all contents in the given tag
-- type
processHTML
    :: String       -- ^ tag type
    -> Text         -- ^ html
    -> [Text]
processHTML tag = mapMaybe getTag
               . H.universeTree
               . H.tagTree
               . cleanTags
               . H.parseTags
  where
    getTag :: TagTree Text -> Maybe Text
    getTag (TagBranch n _ ts) = H.renderTree ts <$ guard (n == T.pack tag)
    getTag _                  = Nothing

-- | Some days, including:
--
-- * 2015 Day 6 Part 1
-- * 2016 Day 2 Part 2
--
-- Have malformed HTML tags; the first has @<p></code>@ and the second has
-- @<span></title>@.  This function cleans up all tags so that any closing
-- tags ignore their actual tag type and instead close the last opened tag
-- (if there is any).  If no tag is currently open then it just leaves it
-- unchanged.
cleanTags
    :: [H.Tag str]
    -> [H.Tag str]
cleanTags = flip evalState [] . mapM go
  where
    go t = case t of
      H.TagOpen n _ -> t <$ modify (n:)
      H.TagClose _  -> get >>= \case
        []   -> pure t
        m:ms -> H.TagClose m <$ put ms
      _             -> pure t
