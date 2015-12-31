{-# LANGUAGE OverloadedStrings #-}
module Client.GuiUtils where
import Haste.App
import Haste.DOM
import Haste.Events
import Control.Monad (forM, void)
import System.IO.Unsafe

-- | Create a new, labeled text field.
newTextField :: String -- ^ Label caption
             -> String -- ^ Text field identifier
             -> String -- ^ Text field type; password/text/number, etc.
             -> String -- ^ Initial value
             -> Client (Elem, Client String)
newTextField caption ident type_ def = do
  label <- newElem "label" `with` [ "textContent" =: caption
                                  , "for" =: ident
                                  , style "margin" =: "0.5em" ]
  e <- newElem "input" `with` [ "type" =: type_
                              , "id" =: ident
                              , "value" =: def
                              , style "margin" =: "0.5em"
                              , style "width" =: "15em"]
  box <- newElem "div" `with` [ children [label, e]
                              , style "text-align" =: "right"
                              , style "margin-right" =: "0.5em"]
  return (box, getProp e "value")

-- | Create a dropdown menu.
dropdown :: String -- ^ Caption for dropdown box
         -> String -- ^ Identifier for box
         -> [(String, String)] -- ^ (caption, value) pairs of box contents
         -> Int    -- ^ Select which option by default?
         -> Client (Elem, Client String)
dropdown caption ident es ix = liftIO $ do
  label <- newElem "label" `with`
    [ "textContent" =: caption
    , "for" =: ident
    , style "margin" =: "0.5em" ]
  opts <- forM es $ \(lbl, val) ->
    newElem "option" `with` [ "value" =: val , "textContent" =: lbl]
  sel <- newElem "select" `with`
    [ "id" =: ident
    , style "margin" =: "0.5em"
    , style "width" =: "15.36em"
    , children opts
    , "selectedIndex" =: show ix ]
  box <- newElem "div" `with`
    [ children [label, sel]
    , style "text-align" =: "right"
    , style "margin-right" =: "0.5em" ]
  return (box, getProp sel "value")

-- | Pre-created "saving..." overlay.
saveOverlay :: Elem

-- | Change the message currently displayed on the save overlay.
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
  void $ overlay `onEvent` Click $ \_ -> do
    deleteChild documentBody outer
    setProp msg "textContent" "Saving..."
  return (outer, setProp msg "textContent")
