{-# LANGUAGE CPP, TupleSections #-}
-- | API definitions for info screen server.
module Server.API where
import Data.Word
import Haste.App
#ifndef __HASTE__
import qualified Server.Methods as M
#endif
import Haste.Binary

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a, b, c, d) where
  put (a, b, c, d) = put a >> put b >> put c >> put d
  get = (,,,) <$> get <*> get <*> get <*> get

instance Binary Bool where
  put True  = put (1 :: Word8)
  put False = put (0 :: Word8)
  get = do
    x <- get :: Get Word8
    case x of
      1 -> pure True
      0 -> pure False
      _ -> fail "get: not a Bool"

data API = API
  { randomBooru :: Remote (Server String)
  , setBooruCfg :: Remote (String -> String -> Char -> [String] -> Server ())
  , getBooruCfg :: Remote (Server (String, String, Char, [String]))
  , shouldClientReload :: Remote (Server Bool)
  }

newAPI :: App API
#ifndef __HASTE__
newAPI = API <$> remote M.randomBooru
             <*> remote M.setBooruCfg
             <*> remote M.getBooruCfg
             <*> remote M.shouldClientReload
#else
newAPI = API <$> remote undefined
             <*> remote undefined
             <*> remote undefined
             <*> remote undefined
#endif
