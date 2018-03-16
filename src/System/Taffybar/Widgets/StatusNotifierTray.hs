module System.Taffybar.Widgets.StatusNotifierTray where

import           Control.Monad
import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe
import           Data.Word
import           Foreign.C.Types (CUChar(..))
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import qualified Graphics.UI.Gtk as Gtk
import           StatusNotifier.Host.Service as H

sampleBits :: Int
sampleBits = 8

hasAlpha :: Bool
hasAlpha = True

colorspace :: Gtk.Colorspace
colorspace = Gtk.ColorspaceRgb

buildTray :: IO Gtk.Widget
buildTray = do
  box <- Gtk.hBoxNew False 5
  let updateHandler ItemAdded info =
        do
          putStrLn "Handling new item"
          pixBuf <- getPixBufFromInfo info
          img <- Gtk.imageNew
          Gtk.imageSetFromPixbuf img pixBuf
          Gtk.widgetShowAll img
          Gtk.boxPackStart box img Gtk.PackNatural 0
      updateHandler IconUpdated info =
        do
          putStrLn "Handling icon update"
          pixBuf <- getPixBufFromInfo info
          return ()
      updateHandler _ _ = return ()
      getPixBufFromInfo ItemInfo { iconPixmaps = pixmaps } = do
        let (width, height, pixmap) = head pixmaps
            pixelsPerRow = fromIntegral width
            bytesPerPixel = 4
            rowStride = pixelsPerRow * bytesPerPixel
            finish ptr = do
              Gtk.pixbufNewFromData (castPtr ptr) colorspace hasAlpha sampleBits
                   (fromIntegral width) (fromIntegral height) rowStride
        unsafeUseAsCString pixmap finish
  join $ H.build H.defaultParams { uniqueIdentifier = "taffybar"
                                 , handleUpdate = (fmap . fmap) Gtk.postGUIAsync updateHandler
                                 }
  return $ Gtk.toWidget box
