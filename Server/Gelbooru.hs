{-# LANGUAGE OverloadedStrings #-}
-- | Search the Gelbooru imageboard for images.
module Server.Gelbooru
  ( -- * Configuration and authentication
    User, Password, BooruConfig
  , cfgRating, cfgLimit
  , booruLogin
    -- * Searching and inspecting image metadata
  , ImageInfo (..), Rating (..), Tag, Page, URL
  , booruSearch
  , booruDownload
  ) where
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intercalate)
import Data.String (IsString)
import Network.HTTP.Conduit
import Network.HTTP.Types
import Text.XML.Light

-- | Rating of the work safety of an image.
data Rating = Safe | Questionable | Explicit | Any
  deriving (Ord, Eq)

instance Show Rating where
  show Safe         = "s"
  show Questionable = "q"
  show Explicit     = "e"
  show Any          = "a"

instance Read Rating where
  readsPrec _ ('s':s) = [(Safe, s)]
  readsPrec _ ('q':s) = [(Questionable, s)]
  readsPrec _ ('e':s) = [(Explicit, s)]
  readsPrec _ ('a':s) = [(Any, s)]
  readsPrec _ _       = []

-- | Description of a Gelbooru image. Heights and widths are given in pixels.
data ImageInfo = ImageInfo {
    imgURL          :: URL,
    imgWidth        :: Int,
    imgHeight       :: Int,
    imgSampleURL    :: URL,
    imgSampleWidth  :: Int,
    imgSampleHeight :: Int,
    imgTags         :: [Tag],
    imgMD5          :: String,
    imgRating       :: Rating
  } deriving (Show, Eq)

type URL = String
type Tag = String
type User = String
type Password = String
type Page = Int

-- | A Gelbooru configuration.
data BooruConfig = BooruConfig
  { cfgAuthCookies :: CookieJar
  , cfgConnMgr     :: Manager
    -- | Only include results with this rating.
    --   Default: @Safe@
  , cfgRating      :: Rating
    -- | Limit on the number of posts to fetch. Gelbooru imposes a hard
    --   limit of 100 posts.
    --   Default: @20@
  , cfgLimit       :: Int
  }

defRating :: Rating
defRating = Safe

defLimit :: Int
defLimit = 20

-- | Login to Gelbooru using a user name and password.
--   Returns @Nothing@ if login fails.
booruLogin :: User -> Password -> IO (Maybe BooruConfig)
booruLogin user pass = do
    urlreq <- parseUrl url
    let req = urlreq
          { method = "POST"
          , requestBody = RequestBodyLBS $ BS.concat
              [ "user="
              , BS.pack user
              , "&pass="
              , BS.pack pass
              , "&submit="
              , "Log+in"
              ]
          , requestHeaders =
              [ (hReferer, url)
              , ("Origin", "gelbooru.com")
              , (hContentType, "application/x-www-form-urlencoded")
              ] ++ requestHeaders urlreq
          }
    mgr <- newManager tlsManagerSettings
    res <- httpLbs req mgr
    let cj = responseCookieJar res
    case (statusIsSuccessful (responseStatus res), destroyCookieJar cj) of
      (True, (_:_)) -> return $ Just $ BooruConfig cj mgr defRating defLimit
      _             -> return Nothing
  where
    url :: IsString a => a
    url = "http://gelbooru.com/index.php?page=account&s=login&code=00"

-- | Perform an image search using the given configuration and tags.
booruSearch :: BooruConfig -> [Tag] -> Page -> IO [ImageInfo]
booruSearch cfg tags page = do
    urlreq <- parseUrl url
    let req = urlreq {cookieJar = Just $ cfgAuthCookies cfg}
    res <- httpLbs req (cfgConnMgr cfg)
    if statusIsSuccessful (responseStatus res)
      then do
        case parseXMLDoc (responseBody res) of
          Just (Element _ _ is _) -> return [x | Right x <- map mkImageInfo is]
          _                       -> return []
      else return []
  where
    mkImageInfo (Elem (Element (QName "post" _ _) attrs _ _)) = do
      ImageInfo <$> lookupKey "file_url" attrs
                <*> readKey "width" attrs
                <*> readKey "height" attrs
                <*> lookupKey "sample_url" attrs
                <*> readKey "sample_width" attrs
                <*> readKey "sample_height" attrs
                <*> fmap words (lookupKey "tags" attrs)
                <*> lookupKey "md5" attrs
                <*> readKey "rating" attrs
    mkImageInfo x = Left $ "Got bad XML: `" ++ show x ++ "'"
    url = concat
      [ "http://gelbooru.com/index.php?page=dapi&s=post&q=index"
      , "&pid=", show page
      , "&limit=", show (cfgLimit cfg)
      , "&tags=", intercalate "%20" (addRating (cfgRating cfg) tags)
      ]

-- | Download an image from Gelbooru using a previously established session.
booruDownload :: BooruConfig -> FilePath -> URL -> IO ()
booruDownload cfg file url = do
  urlreq <- parseUrl url
  let req = urlreq {cookieJar = Just $ cfgAuthCookies cfg}
  res <- httpLbs req (cfgConnMgr cfg)
  BS.writeFile file (responseBody res)

addRating :: Rating -> [Tag] -> [Tag]
addRating Safe         = ("rating:safe" :)
addRating Questionable = ("rating:questionable" :)
addRating Explicit     = ("rating:explicit" :)
addRating Any          = id

listToEither :: String -> [a] -> Either String a
listToEither _ (x:_) = Right x
listToEither s _     = Left s

lookupKey :: String -> [Attr] -> Either String String
lookupKey k (Attr k' v : as)
  | qName k' == k = Right v
  | otherwise     = lookupKey k as
lookupKey k _     = Left $ "lookupKey: no attribute `" ++ k ++ "'"

readKey :: Read a => String -> [Attr] -> Either String a
readKey k as = do
  v <- lookupKey k as
  listToEither (readfail v) (fmap fst $ reads v)

readfail :: String -> String
readfail x = "readKey: couldn't read `" ++ x ++ "'"
