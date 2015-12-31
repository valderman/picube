{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Client.Main where
import Haste (setTimer, Interval (..))
import Haste.App
import Haste.Deck
import Haste.DOM
import Haste.Events
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
textStyle = fontSize (Pt 40) . font "verdana"

slideshow :: Client () -> Deck -> Int -> Int -> Int -> Client ()
slideshow cb d ns t = go
  where
    go ix = void . setTimer (Once t) $ do
      let ix' = (ix `rem` ns) + 1
      cb
      goto d ix'
      go ix'

slideDuration :: Int
slideDuration = 20

-- NOTE: make sure to call @removeItem "reload"@ iff
-- @getItem "reload" == pure False@!
reloadPage :: Client ()
reloadPage = liftIO $ setItem "reload" False >> reload

clientMain :: API -> Client ()
clientMain api@(API {..}) = do
  r <- liftIO $ getItem "reload"
  case r of
    Right False -> liftIO $ removeItem "reload"
    _           -> reloadPage
  set documentBody [ style "background-color" =: "black"
                   , style "color"            =: "white"]
  (cb, booruFrame) <- newBooruFrame api
  let slides =
        [ centered . verticallyCentered . textStyle $ clockFrame
        , centered booruFrame
        ]
  d <- liftIO $ present def {transition = fade} slides
  slideshow cb d (length slides) (slideDuration*1000) 1
