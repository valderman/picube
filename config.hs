{-# LANGUAGE OverloadedStrings #-}
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
    (u, p, r, t) <- onServer $ getBooruCfg api
    (user, getUser) <- newTextField "Gelbooru username" "user" False u
    (pass, getPass) <- newTextField "Gelbooru password" "pass" True p
    (tags, getTags) <- newTextField "Gelbooru tags" "tags" False (unwords t)
    btn <- newElem "button" `with` ["textContent" =: "Save"]
    box <- newElem "div" `with` [ children [user, pass, tags, btn]
                                , style "width" =: "23em"
                                , style "border" =: "1px solid black"]
    btn `onEvent` Click $ \_ -> do
      u' <- getUser
      p' <- getPass
      t' <- getTags
      onServer $ setBooruCfg api <.> u' <.> p' <.> r <.> words t'
    setChildren documentBody [box]
