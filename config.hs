{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Haste.App
import Haste.DOM
import Haste.Events
import Server.API

newTextField :: String -> String -> Bool -> String -> Client (Elem, Client String)
newTextField caption ident pass def = do
  label <- newElem "label" `with` [ "textContent" =: caption
                                  , "for" =: ident
                                  , style "padding" =: "1em" ]
  e <- newElem "input" `with` [ "type" =: if pass then "password" else "text"
                              , "id" =: ident
                              , "value" =: def]
  box <- newElem "div" `with` [children [label, e]]
  return (box, getProp e "value")

main :: IO ()
main = runApp defaultConfig $ do
  api <- newAPI
  runClient $ do
    cfg@(Config {..}) <- onServer $ getCfg api
    (user, getUser) <- newTextField "Gelbooru username" "user" False cfgBooruUsername
    (pass, getPass) <- newTextField "Gelbooru password" "pass" True cfgBooruPassword
    (tags, getTags) <- newTextField "Gelbooru tags" "tags" False (unwords cfgBooruTags)
    btn <- newElem "button" `with` ["textContent" =: "Save"]
    box <- newElem "div" `with` [ children [user, pass, tags, btn]
                                , style "width" =: "23em"
                                , style "border" =: "1px solid black"]
    btn `onEvent` Click $ \_ -> do
      u' <- getUser
      p' <- getPass
      t' <- getTags
      let cfg' = cfg
            { cfgBooruUsername = u'
            , cfgBooruPassword = p'
            , cfgBooruTags = words t'
            }
      onServer $ setCfg api <.> cfg'
    setChildren documentBody [box]
