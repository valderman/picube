module Server.Methods where
import Haste.App
import Server.Config
import qualified Server.Gelbooru as G
import System.Random
import Control.Concurrent
import System.IO.Unsafe

{-# NOINLINE reloadMVar #-}
reloadMVar :: MVar Bool
reloadMVar = unsafePerformIO $ newEmptyMVar

-- | Get a random image from Gelbooru with the given tags.
--   We need to download the image, to avoid Gelbooru going all "OMG NO INLINE
--   LINKS PLZ" on us.
randomBooru :: Server String
randomBooru = liftIO $ do
    cfg <- getConfig
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
            ix <- randomRIO (0, length res-1)
            G.booruDownload booru basefile (G.imgSampleURL (res !! ix))
            return (basefile ++ "?" ++ show ix)
      _ -> do
        return []
  where
    basefile = "booru.jpg"

-- | Update the server's Gelbooru configuration.
setBooruCfg :: G.User -> G.Password -> Char -> [G.Tag] -> Server ()
setBooruCfg user pass rating tags = liftIO $ do
  updateConfig $ \cfg -> cfg
    { cfgBooruUsername = user
    , cfgBooruPassword = pass
    , cfgBooruRating = rating
    , cfgBooruTags = tags
    }
  putMVar reloadMVar True

-- | Get the server's Gelbooru configuration.
getBooruCfg :: Server (G.User, G.Password, Char, [G.Tag])
getBooruCfg = liftIO $ do
  cfg <- getConfig
  return ( cfgBooruUsername cfg
         , cfgBooruPassword cfg
         , cfgBooruRating cfg
         , cfgBooruTags cfg)

-- | Should the client reload to apply new configuration?
--   Beware, if more than one client is awaiting reload, only one of them
--   will see the reload notification; the rest will have to wait until the
--   *next* reload notification and so on.
shouldClientReload :: Server Bool
shouldClientReload = liftIO $ takeMVar reloadMVar
