{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Config (configMain) where
import Control.Monad
import Data.IORef
import Haste.App.Standalone
import Haste.DOM
import Haste.Events
import Server.API
import Client.GuiUtils

-- | Save configuration with pretty confirmation screen.
saveConfig :: API -> IORef Config -> Config -> Client ()
saveConfig api cfgref cfg = do
  appendChild documentBody saveOverlay
  onServer $ setCfg api <.> cfg
  liftIO $ writeIORef cfgref cfg
  setOverlayMessage "Configuration saved, click to continue"

-- | Gelbooru configuration.
booruConfigBox :: API -> IORef Config -> Client Elem
booruConfigBox api cfgref = do
  (Config {..}) <- liftIO $ readIORef cfgref
  (user, getUser) <- newTextField "Gelbooru username:" "user" "text"
                                  cfgBooruUsername
  (pass, getPass) <- newTextField "Gelbooru password:" "pass" "password"
                                  cfgBooruPassword
  (rating, getRating) <- dropdown "Rating:" "rating" ratings
                                  (selIx cfgBooruRating)
  (tags, getTags) <- newTextField "Gelbooru tags:" "tags" "text"
                                  (unwords cfgBooruTags)
  (fmt, getFmt) <- dropdown "Format:" "format" formats
                                  (fromEnum cfgBooruFormat)
  hdr <- newElem "h3" `with`
    [ "textContent" =: "Gelbooru configuration"
    , style "margin" =: "0.5em"]
  btn <- newElem "button" `with`
    [ "textContent" =: "Save"
    , style "margin" =: "0.5em"]
  box <- newElem "div" `with`
    [ children [hdr, user, pass, rating, tags, fmt, btn]
    , style "display" =: "inline-block"
    , style "border" =: "1px solid black"
    , style "width" =: "100%"
    , style "margin" =: "1em"]
  void $ btn `onEvent` Click $ \_ -> do
    u' <- getUser
    p' <- getPass
    t' <- getTags
    r' <- getRating
    f' <- getFmt
    cfg' <- liftIO $ readIORef cfgref
    let cfg'' = cfg'
          { cfgBooruUsername = u'
          , cfgBooruPassword = p'
          , cfgBooruTags = words t'
          , cfgBooruRating = head r'
          , cfgBooruFormat = toEnum (read f')
          }
    saveConfig api cfgref cfg''
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

formats :: [(String, String)]
formats =
  [ ("Portrait", show $ fromEnum Portrait)
  , ("Landscape", show $ fromEnum Landscape)
  , ("Either", show $ fromEnum Either)]

-- | Display configuration.
displayConfigBox :: API -> IORef Config -> Client Elem
displayConfigBox api cfgref = do
  (Config {..}) <- liftIO $ readIORef cfgref
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
    cfg' <- liftIO $ readIORef cfgref
    let cfg'' = cfg' {cfgSlideDuration = read s'}
    saveConfig api cfgref cfg''
  return box

configMain :: API -> Client ()
configMain api = do
  set documentBody [style "font-family" =: "helvetica"]
  cfgref <- onServer (getCfg api) >>= liftIO . newIORef
  booru <- booruConfigBox api cfgref
  disp <- displayConfigBox api cfgref
  outer <- newElem "div" `with`
    [ children [disp, booru]
    , style "width" =: "30em"]
  setChildren documentBody [outer]
