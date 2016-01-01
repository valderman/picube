{-# LANGUAGE RecordWildCards #-}
module Server.Config where
import Data.IORef
import System.IO.Unsafe
import Control.Exception
import Haste.Binary
import System.IO

instance Binary Config where
  put (Config {..}) = do
    put cfgBooruUsername
    put cfgBooruPassword
    put cfgBooruRating
    put cfgBooruTags
    put cfgBooruPersists
    put cfgSlideDuration
  get = Config <$> get <*> get <*> get <*> get <*> get
               <*> get

data Config = Config
  { -- | Username for Gelbooru.
    cfgBooruUsername :: String
    -- | Password for Gelbooru.
  , cfgBooruPassword :: String
    -- | Rating filter for Gelbooru.
  , cfgBooruRating   :: Char
    -- | Tag filter for Gelbooru.
  , cfgBooruTags     :: [String]
    -- | How many slide updates should Gelbooru images persist before a new one
    --   is fetched? A new image will be randomly selected every
    --   @cfgSlideDuration * cfgBooruPersists * num_slides@ seconds.
  , cfgBooruPersists :: Int
    -- | How many seconds between slide updates?
  , cfgSlideDuration :: Int
  } deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config
  { cfgBooruUsername = ""
  , cfgBooruPassword = ""
  , cfgBooruRating   = 's'
  , cfgBooruTags     = []
  , cfgBooruPersists = 10
  , cfgSlideDuration = 30
  }

configFile :: FilePath
configFile = "../config.txt"

{-# NOINLINE configRef #-}
configRef :: IORef Config
configRef = unsafePerformIO $ do
  cfg <- catch (tryRead configFile)
               (\(SomeException _) -> pure defaultConfig)
  newIORef cfg

tryRead :: FilePath -> IO Config
tryRead fp = do
  putStrLn "reading file"
  withFile fp ReadMode $ \h -> do
    s <- hGetContents h
    putStrLn s
    case reads s of
      ((cfg, _) : _) -> return cfg
      _              -> fail "unable to parse config"

updateConfig :: (Config -> Config) -> IO ()
updateConfig f = do
  atomicModifyIORef' configRef (\c -> (f c, ()))
  saveConfig

getConfig :: IO Config
getConfig = readIORef configRef

saveConfig :: IO ()
saveConfig = readIORef configRef >>= writeFile configFile . show
