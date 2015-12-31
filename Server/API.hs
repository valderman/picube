{-# LANGUAGE CPP, TupleSections #-}
-- | API definitions for info screen server.
module Server.API where
import Haste.App
#ifndef __HASTE__
import qualified Server.Methods as M
#endif
import Haste.Binary

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a, b, c, d) where
  put (a, b, c, d) = put a >> put b >> put c >> put d
  get = (,,,) <$> get <*> get <*> get <*> get

data API = API
  { randomBooru :: Remote (Server String)
  , setBooruCfg :: Remote (String -> String -> Char -> [String] -> Server ())
  , getBooruCfg :: Remote (Server (String, String, Char, [String]))
  }

newAPI :: App API
#ifndef __HASTE__
newAPI = API <$> remote M.randomBooru
             <*> remote M.setBooruCfg
             <*> remote M.getBooruCfg
#else
newAPI = API <$> remote undefined
             <*> remote undefined
             <*> remote undefined
#endif
