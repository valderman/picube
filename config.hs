{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Control.Monad
import Haste.App
import Haste.DOM
import Haste.Events
import Server.API
import Client.GuiUtils

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
  (rating, getRating) <- dropdown "Rating:" "rating" ratings
                                  (selIx cfgBooruRating)
  (tags, getTags) <- newTextField "Gelbooru tags:" "tags" "text"
                                  (unwords cfgBooruTags)
  hdr <- newElem "h3" `with`
    [ "textContent" =: "Gelbooru configuration"
    , style "margin" =: "0.5em"]
  btn <- newElem "button" `with`
    [ "textContent" =: "Save"
    , style "margin" =: "0.5em"]
  box <- newElem "div" `with`
    [ children [hdr, user, pass, rating, tags, btn]
    , style "display" =: "inline-block"
    , style "border" =: "1px solid black"
    , style "width" =: "100%"
    , style "margin" =: "1em"]
  void $ btn `onEvent` Click $ \_ -> do
    u' <- getUser
    p' <- getPass
    t' <- getTags
    r' <- getRating
    let cfg' = cfg
          { cfgBooruUsername = u'
          , cfgBooruPassword = p'
          , cfgBooruTags = words t'
          , cfgBooruRating = head r'
          }
    saveConfig api cfg'
  return box

selIx :: Char -> Int
selIx 's' = 0
selIx 'q' = 1
selIx 'e' = 2
selIx _   = 3

ratings :: [(String, String)]
ratings =
  [ ("Safe", "s")
  , ("Questionable", "q")
  , ("Explicit", "e")
  , ("Any", "a")]

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
