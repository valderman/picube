{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Control.Monad
import Haste.App
import Haste.DOM
import Haste.Events
import System.IO.Unsafe
import Server.API

newTextField :: String -> String -> String -> String -> Client (Elem, Client String)
newTextField caption ident type_ def = do
  label <- newElem "label" `with` [ "textContent" =: caption
                                  , "for" =: ident
                                  , style "margin" =: "0.5em" ]
  e <- newElem "input" `with` [ "type" =: type_
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
  void $ overlay `onEvent` Click $ \_ -> deleteChild documentBody outer
  return (outer, setProp msg "textContent")

-- | Save configuration with pretty confirmation screen.
saveConfig :: API -> Config -> Client ()
saveConfig api cfg = do
  appendChild documentBody saveOverlay
  onServer $ setCfg api <.> cfg
  setOverlayMessage "Configuration saved, click to continue"

-- | Gelbooru configuration.
booruConfigBox :: API -> Config -> Client Elem
booruConfigBox api cfg@(Config {..}) = do
  (user, getUser) <- newTextField "Gelbooru username:" "user" "text"
                                  cfgBooruUsername
  (pass, getPass) <- newTextField "Gelbooru password:" "pass" "password"
                                  cfgBooruPassword
  (tags, getTags) <- newTextField "Gelbooru tags:" "tags" "text"
                                  (unwords cfgBooruTags)
  hdr <- newElem "h3" `with`
    [ "textContent" =: "Gelbooru configuration"
    , style "margin" =: "0.5em"]
  btn <- newElem "button" `with`
    [ "textContent" =: "Save"
    , style "margin" =: "0.5em"]
  box <- newElem "div" `with`
    [ children [hdr, user, pass, tags, btn]
    , style "display" =: "inline-block"
    , style "border" =: "1px solid black"
    , style "width" =: "100%"
    , style "margin" =: "1em"]
  void $ btn `onEvent` Click $ \_ -> do
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

-- | Display configuration.
displayConfigBox :: API -> Config -> Client Elem
displayConfigBox api cfg@(Config {..}) = do
  (delay, getSecs) <- newTextField "Seconds between frames:" "s" "number"
                                   (show cfgSlideDuration)
  hdr <- newElem "h3" `with`
    [ "textContent" =: "Display configuration"
    , style "margin" =: "0.5em"]
  btn <- newElem "button" `with`
    [ "textContent" =: "Save"
    , style "margin" =: "0.5em"]
  box <- newElem "div" `with`
    [ children [hdr, delay, btn]
    , style "display" =: "inline-block"
    , style "border" =: "1px solid black"
    , style "width" =: "100%"
    , style "margin" =: "1em"]
  void $ btn `onEvent` Click $ \_ -> do
    s' <- getSecs
    let cfg' = cfg {cfgSlideDuration = read s'}
    saveConfig api cfg'
  return box

main :: IO ()
main = runApp defaultConfig $ do
  api <- newAPI
  runClient $ do
    set documentBody [style "font-family" =: "helvetica"]
    cfg <- onServer $ getCfg api
    booru <- booruConfigBox api cfg
    disp <- displayConfigBox api cfg
    outer <- newElem "div" `with`
      [ children [disp, booru]
      , style "width" =: "30em"]
    setChildren documentBody [outer]
