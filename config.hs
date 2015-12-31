{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Haste.App
import Haste.DOM
import Haste.Events
import Server.API
import System.IO.Unsafe

newTextField :: String -> String -> Bool -> String -> Client (Elem, Client String)
newTextField caption ident pass def = do
  label <- newElem "label" `with` [ "textContent" =: caption
                                  , "for" =: ident
                                  , style "margin" =: "0.5em" ]
  e <- newElem "input" `with` [ "type" =: if pass then "password" else "text"
                              , "id" =: ident
                              , "value" =: def
                              , style "margin" =: "0.5em"]
  box <- newElem "div" `with` [ children [label, e]
                              , style "text-align" =: "right"
                              , style "margin-right" =: "0.5em"]
  return (box, getProp e "value")

saveOverlay :: Elem
setOverlayMessage :: String -> Client ()
(saveOverlay, setOverlayMessage) = unsafePerformIO $ do
  overlay <- newElem "div" `with`
    [ style "width" =: "100%"
    , style "height" =: "100%"
    , style "top" =: "0px"
    , style "left" =: "0px"
    , style "position" =: "fixed"
    , style "opacity" =: "0.5"
    , style "text-align" =: "center"
    , style "vertical-align" =: "middle"
    , style "display" =: "table-cell"
    , style "background-color" =: "black"
    ]
  msg <- newElem "h3" `with`
    [ style "text-align" =: "center"
    , style "width" =: "100%"
    , style "display" =: "inline-block"
    , style "color" =: "white"
    , style "position" =: "relative"
    , "textContent" =: "Saving..."
    ]
  outer <- newElem "div" `with` [children [overlay, msg]]
  overlay `onEvent` Click $ \_ -> deleteChild documentBody outer
  return (outer, setProp msg "textContent")

saveConfig :: API -> Config -> Client ()
saveConfig api cfg = do
  appendChild documentBody saveOverlay
  onServer $ setCfg api <.> cfg
  setOverlayMessage "Configuration saved, click to continue"

booruConfigBox :: API -> Config -> Client Elem
booruConfigBox api cfg@(Config {..}) = do
  (user, getUser) <- newTextField "Gelbooru username:" "user" False cfgBooruUsername
  (pass, getPass) <- newTextField "Gelbooru password:" "pass" True cfgBooruPassword
  (tags, getTags) <- newTextField "Gelbooru tags:" "tags" False (unwords cfgBooruTags)
  hdr <- newElem "h3" `with` [ "textContent" =: "Gelbooru configuration"
                             , style "margin" =: "0.5em"]
  btn <- newElem "button" `with` [ "textContent" =: "Save"
                                 , style "margin" =: "0.5em"]
  box <- newElem "div" `with` [ children [hdr, user, pass, tags, btn]
                              , style "display" =: "inline-block"
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
    saveConfig api cfg'
  return box

main :: IO ()
main = runApp defaultConfig $ do
  api <- newAPI
  runClient $ do
    set documentBody [style "font-family" =: "helvetica"]
    cfg <- onServer $ getCfg api
    booru <- booruConfigBox api cfg
    setChildren documentBody [booru]
