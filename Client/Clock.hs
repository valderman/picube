module Client.Clock where
import Control.Monad
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Haste
import Haste.Deck
import Haste.DOM

-- | Displays a digital clock.
clockFrame :: Slide
clockFrame = lift $ do
    e <- newElem "div" `with` [style "display" =: "inline-block"]
    updateTime e
    void $ setTimer (Repeat 30000) (updateTime e)
    return e
  where
    updateTime e = do
      t <- getCurrentTime
      z <- getCurrentTimeZone
      let t' = utcToLocalTime z t
      setProp e "textContent" (formatTime defaultTimeLocale "%R" t')
