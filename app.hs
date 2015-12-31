{-# LANGUAGE CPP #-}
module Main where
import Haste.App
import Server.API
#ifdef __HASTE__
import Client.Main
#else
clientMain :: API -> Client ()
clientMain _ = return ()
#endif

main :: IO ()
main = runApp defaultConfig $ newAPI >>= runClient . clientMain
