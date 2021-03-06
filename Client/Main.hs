{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Client.Main where
import Haste (setTimer, Interval (..))
import Haste.App
import Haste.App.Concurrent
import Haste.Deck
import Haste.DOM
import Haste.Foreign
import Haste.LocalStorage
import Control.Monad
import Client.Clock
import Server.API

reload :: IO ()
reload = ffi "(function(){location.reload();})"

-- | Create a new frame for a random Gelbooru image.
--   Images are picked at random from the 100 first search results for the
--   tags given in the server config.
newBooruFrame :: API -> Client (Client (), Slide)
newBooruFrame api = do
    img <- newElem "img" `with` [ style "width" =: "100%"
                                , style "display" =: "inline-block"]
    return (mkCB img, lift $ return img)
  where
    mkCB img = do
      url <- onServer $ randomBooru api
      set img ["src" =: url]

textStyle :: Slide -> Slide
textStyle = fontSize (Pt 40) . font "helvetica"

slideshow :: Client () -> Deck -> Int -> Int -> Int -> Client ()
slideshow cb d ns t = go
  where
    go ix = void . setTimer (Once t) $ do
      let ix' = (ix `rem` ns) + 1
      cb
      goto d ix'
      go ix'

-- NOTE: make sure to call @removeItem "reload"@ iff
-- @getItem "reload" == pure False@!
reloadPage :: Client ()
reloadPage = liftIO $ setItem "reload" False >> reload

clientMain :: API -> Client ()
clientMain api@(API {..}) = do
  -- Always reload on first boot
  r <- liftIO $ getItem "reload"
  case r of
    Right False -> liftIO $ removeItem "reload"
    _           -> reloadPage

  -- Set up background and page structure
  set documentBody [ style "background-color" =: "black"
                   , style "color"            =: "white"]
  (cb, booruFrame) <- newBooruFrame api
  let slides =
        [ centered . verticallyCentered . textStyle $ clockFrame
        , centered booruFrame
        ]

  -- Get config and reload when it changes
  Config {..} <- onServer getCfg
  fork $ awaitReconfiguration api
  
  -- Start slideshow
  d <- liftIO $ present def {transition = fade} slides
  slideshow cb d (length slides) (cfgSlideDuration*1000) 1

-- | Reload page when reconfiguration happens.
awaitReconfiguration :: API -> Client ()
awaitReconfiguration api = do
  shouldReload <- onServer $ shouldClientReload api
  if shouldReload
    then reloadPage
    else awaitReconfiguration api
