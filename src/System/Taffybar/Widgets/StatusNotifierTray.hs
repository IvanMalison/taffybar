module System.Taffybar.Widgets.StatusNotifierTray where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.ByteString.Unsafe
import qualified Data.Text as T
import           Foreign.Ptr
import qualified Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk.Gdk.Display
import           Graphics.UI.Gtk.General.IconTheme
import           StatusNotifier.Host.Service as H
import           Text.Printf
import           XMonad.Core (whenJust)

sampleBits :: Int
sampleBits = 8

hasAlpha :: Bool
hasAlpha = True

colorspace :: Gtk.Colorspace
colorspace = Gtk.ColorspaceRgb

getIconByName :: Int -> String -> Maybe FilePath -> IO (Maybe Gtk.Pixbuf)
getIconByName size name themePath = do
  putStrLn "hey im here2222"
  newTheme <- iconThemeNew
  defaultTheme <- iconThemeGetDefault
  -- (filePaths, _) <- iconThemeGetSearchPath defaultTheme
  -- whenJust themePath $
  --          iconThemeAppendSearchPath newTheme
  -- mapM_ (iconThemeAppendSearchPath newTheme) filePaths
  let newTheme = defaultTheme
  _ <- runMaybeT $ do
      d <- MaybeT displayGetDefault
      s <- lift $ displayGetScreen d 0
      lift $ iconThemeSetScreen newTheme s
  let panelName = printf "%s-panel" name
  hasPanelIcon <- iconThemeHasIcon newTheme panelName
  let targetName = if hasPanelIcon then panelName else name
      nameText = T.pack targetName
  iconThemeLoadIcon newTheme nameText size (IconLookupGenericFallback)

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
          -- putStrLn "Handling icon update"
          -- pixBuf <- getPixBufFromInfo info
          return ()
      updateHandler NameUpdated info = return ()
      updateHandler _ _ = return ()
      getPixBufFromInfo info = do
        maybePB <- getPixBufFromName info
        maybe (getPixBufFromPixmaps info) return maybePB
      getPixBufFromName ItemInfo { iconName = name, iconThemePath = themePath } =
        getIconByName 25 name themePath
      getPixBufFromPixmaps ItemInfo { iconPixmaps = pixmaps } = do
        let (width, height, pixmap) = head pixmaps
            pixelsPerRow = fromIntegral width
            bytesPerPixel = 4
            rowStride = pixelsPerRow * bytesPerPixel
            finish ptr = do
              Gtk.pixbufNewFromData (castPtr ptr) colorspace hasAlpha sampleBits
                   (fromIntegral width) (fromIntegral height) rowStride
        unsafeUseAsCString pixmap finish
  _ <- join $ H.build H.defaultParams
       { uniqueIdentifier = "taffybar"
       , handleUpdate = (fmap . fmap) Gtk.postGUIAsync updateHandler
       }
  return $ Gtk.toWidget box
