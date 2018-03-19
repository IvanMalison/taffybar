module System.Taffybar.Widgets.StatusNotifierTray where

import           Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Trans.Reader
import           DBus.Client
import           Data.ByteString.Unsafe
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import           Foreign.Ptr
import qualified Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk.Gdk.Display
import           Graphics.UI.Gtk.General.IconTheme
import           StatusNotifier.Host.Service as H
import qualified StatusNotifier.Menu.Service as M
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
  -- TODO: make theming work
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

data ItemContext = ItemContext
  { contextInfo :: H.ItemInfo
  , menuMVar :: MV.MVar (Map.Map Int32 M.MenuEntry)
  , contextImage :: Gtk.Image
  , contextWidget :: Gtk.EventBox
  }

fillMenu :: Gtk.MenuClass menu => ItemContext ->  menu -> IO ()
fillMenu ItemContext { menuMVar = menuVar } menu = do
  menuEntryMap <- MV.readMVar menuVar
  putStrLn "hey"
  print menuEntryMap
  let mroot = Map.lookup 0 menuEntryMap
      childrenIDs = fromMaybe [] $ M.childIDs <$> mroot
      childrenEntries = catMaybes $ map (flip Map.lookup menuEntryMap) childrenIDs
      addItemForEntry M.MenuEntry { M.label = labelText } =
        Gtk.menuItemNewWithLabel labelText >>= Gtk.menuShellAppend menu
  mapM_ addItemForEntry childrenEntries

buildTray :: IO Gtk.Widget
buildTray = do
  client <- connectSession
  box <- Gtk.hBoxNew False 5
  widgetMap <- MV.newMVar Map.empty

  let getContext name = Map.lookup name <$> MV.readMVar widgetMap

      updateHandler ItemAdded
                    info@ItemInfo { menuPath = pathForMenu
                                  , itemServiceName = serviceName
                                  } =
        do
          pixBuf <- getPixBufFromInfo info
          img <- Gtk.imageNew
          ebox <- Gtk.eventBoxNew
          Gtk.imageSetFromPixbuf img pixBuf
          Gtk.containerAdd ebox img
          Gtk.widgetShowAll ebox
          Gtk.boxPackStart box ebox Gtk.PackNatural 0
          (menuVar, _) <- runReaderT (M.buildMenu M.defaultMenuParams)
                          (client, serviceName, pathForMenu)
          menu <- Gtk.menuNew
          putStrLn "yooooo"
          let context =
                ItemContext { contextInfo = info
                            , menuMVar = menuVar
                            , contextImage = img
                            , contextWidget = ebox
                            }
          fillMenu context menu
          Gtk.menuAttachToWidget menu ebox
          Gtk.on ebox Gtk.buttonPressEvent $ liftIO $
             Gtk.menuPopup menu Nothing >> return True
          MV.modifyMVar_ widgetMap $ return . (Map.insert serviceName context)

      updateHandler ItemRemoved ItemInfo { itemServiceName = name }
        = putStrLn "here" >> getContext name >>= removeWidget
        where removeWidget Nothing = putStrLn "Tried to remove widget for which we have no icon"
              removeWidget (Just (ItemContext { contextWidget = widgetToRemove })) =
                do
                  putStrLn "removing widget"
                  Gtk.containerRemove box widgetToRemove
                  MV.modifyMVar_ widgetMap $ return . (Map.delete name)

      updateHandler _ _ = return ()

      getPixBufFromInfo info = do
        maybePB <- getPixBufFromName info
        maybe (getPixBufFromPixmaps info) return maybePB
      getPixBufFromName ItemInfo { iconName = name, iconThemePath = themePath } =
        getIconByName 25 name themePath
      getPixBufFromPixmaps ItemInfo { iconPixmaps = pixmaps } = do
              -- TODO: get rid of the head here
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
       -- TODO: more fine grained approach to making sure things are on the UI thread
       , handleUpdate = (fmap . fmap) Gtk.postGUIAsync updateHandler
       , dbusClient = Just client
       }
  return $ Gtk.toWidget box
