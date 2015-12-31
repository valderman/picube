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
    put cfgUpdateDelay
  get = Config <$> get <*> get <*> get <*> get
               <*> get

data Config = Config
  { cfgBooruUsername :: String
  , cfgBooruPassword :: String
  , cfgBooruRating   :: Char
  , cfgBooruTags     :: [String]
  , cfgUpdateDelay   :: Int
  } deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config
  { cfgBooruUsername = ""
  , cfgBooruPassword = ""
  , cfgBooruRating   = 's'
  , cfgBooruTags     = []
  , cfgUpdateDelay   = 30
  }

{-# NOINLINE configRef #-}
configRef :: IORef Config
configRef = unsafePerformIO $ do
  cfg <- catch (tryRead "config.txt")
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
saveConfig = readIORef configRef >>= writeFile "config.txt" . show
