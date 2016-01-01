module Server.Methods where
import Haste.App
import Server.Config
import qualified Server.Gelbooru as G
import System.Random
import Control.Concurrent
import System.IO.Unsafe
import Data.IORef

{-# NOINLINE reloadMVar #-}
reloadMVar :: MVar Bool
reloadMVar = unsafePerformIO $ newEmptyMVar

{-# NOINLINE booruPersists #-}
booruPersists :: IORef Int
booruPersists = unsafePerformIO $ newIORef 0

-- | Get a random image from Gelbooru with the given tags.
--   We need to download the image, to avoid Gelbooru going all "OMG NO INLINE
--   LINKS PLZ" on us.
randomBooru :: Server String
randomBooru = liftIO $ do
    ps <- readIORef booruPersists
    cfg <- getConfig
    if ps > 0
      then do
        -- Race condition if more than one device is using the same server,
        -- so don't do that.
        writeIORef booruPersists (ps-1)
        return basefile
      else do
        mb <- G.booruLogin (cfgBooruUsername cfg) (cfgBooruPassword cfg)
        case mb of
          Just b -> do
            let booru = b
                  { G.cfgLimit = 100
                  , G.cfgRating = read [cfgBooruRating cfg]
                  }
            res <- G.booruSearch booru (cfgBooruTags cfg) 0
            if null res
              then return ""
              else do
                let res' = filterImgs (cfgBooruFormat cfg) res
                ix <- randomRIO (0, length res'-1)
                G.booruDownload booru basefile (G.imgSampleURL (res' !! ix))
                writeIORef booruPersists (cfgBooruPersists cfg)
                return (basefile ++ "?" ++ show ix)
          _ -> do
            return []
  where
    filterImgs Either = id
    filterImgs Portrait = filter (\i -> G.imgHeight i > G.imgWidth i)
    filterImgs Landscape = filter (\i -> G.imgHeight i < G.imgWidth i)
    basefile = "booru.jpg"

-- | Update the server's configuration.
setCfg :: Config -> Server ()
setCfg cfg = liftIO $ do
  updateConfig $ const cfg
  writeIORef booruPersists 0
  putMVar reloadMVar True

-- | Get the server's configuration.
getCfg :: Server Config
getCfg = liftIO getConfig

-- | Should the client reload to apply new configuration?
--   Beware, if more than one client is awaiting reload, only one of them
--   will see the reload notification; the rest will have to wait until the
--   *next* reload notification and so on.
shouldClientReload :: Server Bool
shouldClientReload = liftIO $ takeMVar reloadMVar
