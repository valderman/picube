module Server.Config where
import Data.IORef
import System.IO.Unsafe
import Control.Exception

data Config = Config
  { cfgBooruUsername :: String
  , cfgBooruPassword :: String
  , cfgBooruRating   :: Char
  , cfgBooruTags     :: [String]
  } deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config
  { cfgBooruUsername = ""
  , cfgBooruPassword = ""
  , cfgBooruRating   = 's'
  , cfgBooruTags     = []
  }

{-# NOINLINE configRef #-}
configRef :: IORef Config
configRef = unsafePerformIO $ do
  cfg <- catch (read <$> readFile "config.txt")
               (\(SomeException _) -> pure defaultConfig)
  newIORef cfg

updateConfig :: (Config -> Config) -> IO ()
updateConfig f = do
  atomicModifyIORef' configRef (\c -> (f c, ()))
  saveConfig

getConfig :: IO Config
getConfig = readIORef configRef

saveConfig :: IO ()
saveConfig = readIORef configRef >>= writeFile "config.txt" . show
