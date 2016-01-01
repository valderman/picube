{-# LANGUAGE CPP, OverloadedStrings #-}
module Main where
import Haste.App.Standalone
import Haste.Foreign
import Server.API
#ifdef __HASTE__
import Client.Main
import Config
#else
clientMain, configMain :: API -> Client ()
clientMain _ = return ()
configMain _ = return ()
#endif

isClient :: IO Bool
isClient = ffi "(function(){return window.location.pathname == '/app.html';})"

main :: IO ()
main = runStandaloneApp $ do
  api <- newAPI
  runClient $ do
    client <- liftIO isClient
    if client
      then clientMain api
      else configMain api
