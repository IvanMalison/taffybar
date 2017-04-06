{-# LANGUAGE FlexibleContexts #-}
-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Taffybar.Systray (
  systrayNew,
  TrayWidgetManager(..),
  newTrayWidgetManager,
  getManagedTrayBuilder
) where

import Control.Monad
import Data.List
import Data.Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Misc.TrayManager
import qualified Control.Concurrent.MVar as MV

trayManagerOnDefaultScreen :: IO TrayManager
trayManagerOnDefaultScreen = do
  trayManager <- trayManagerNew
  Just screen <- screenGetDefault
  _ <- trayManagerManageScreen trayManager screen
  return trayManager

systrayNew :: IO Widget
systrayNew = do
  box <- hBoxNew False 5

  trayManager <- trayManagerOnDefaultScreen

  _ <- on trayManager trayIconAdded $ \w -> do
    widgetShowAll w
    boxPackStart box w PackNatural 0

  _ <- on trayManager trayIconRemoved $ \_ ->
    putStrLn "Tray icon removed"

  widgetShowAll box
  return (toWidget box)

data TrayWidgetManager =
  TrayWidgetManager { widgets :: MV.MVar [Widget]
                    , onTrayAddWidget :: MV.MVar (Widget -> IO ())
                    }

newTrayWidgetManager :: (Widget -> IO ()) -> IO TrayWidgetManager
newTrayWidgetManager onAddAction = do
  widgetsVar <- MV.newMVar []
  onAddVar <- MV.newMVar onAddAction

  trayManager <- trayManagerOnDefaultScreen

  _ <- on trayManager trayIconAdded $ \w -> do
    MV.modifyMVar_ widgetsVar (return . (w:))
    action <- MV.readMVar onAddVar
    action w

  _ <- on trayManager trayIconRemoved $ \w ->
    MV.modifyMVar_ widgetsVar (return . delete w)

  return TrayWidgetManager { widgets = widgetsVar
                           , onTrayAddWidget = onAddVar
                           }

getManagedTrayBuilder :: IO (IO Widget)
getManagedTrayBuilder = do
  widgetManagerVar <- MV.newEmptyMVar
  let buildWidget = do
        isEmpty <- MV.isEmptyMVar widgetManagerVar
        when isEmpty $
             do
               toPut <- newTrayWidgetManager $ const $ return ()
               MV.putMVar widgetManagerVar toPut
        manager <- MV.readMVar widgetManagerVar
        buildManagedWidget manager
  return buildWidget

buildManagedWidget :: TrayWidgetManager -> IO Widget
buildManagedWidget TrayWidgetManager { widgets = widgetsVar
                                     , onTrayAddWidget = onAddVar
                                     } =
  do
    box <- hBoxNew False 5
    let addWidgetToBox widget =
          do
            -- maybe (return ()) (flip containerRemove widget) parent
            widgetShowAll widget
            parent <- widgetGetParent widget
            if isJust parent
            then
              widgetReparent widget box
            else
              containerAdd box widget
    MV.modifyMVar_ onAddVar $ const $ do
                    MV.readMVar widgetsVar >>= mapM_ addWidgetToBox
                    return addWidgetToBox
    widgetShowAll box
    return $ toWidget box
