{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WorkspaceHUD
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------

module System.Taffybar.WorkspaceHUD (
  WWC(..),
  Workspace(..),
  WorkspaceHUDConfig(..),
  WorkspaceContentsController(..),
  WorkspaceWidgetController(..),
  buildWorkspaceHUD,
  buildWorkspaceWidgets,
  buildWorkspaces,
  getWorkspaceToWindows
) where

import qualified Control.Concurrent.MVar as MV
import           Control.Concurrent.STM.TVar
import           Control.Monad
import qualified Data.Char as S
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Layout.Table as T
import           Graphics.X11.Xlib.Extras
import           System.Information.EWMHDesktopInfo
import           System.Taffybar.Pager
import           Text.Printf

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

data Workspace =
  Workspace { workspaceIdx :: WorkspaceIdx
            , workspaceName :: String
            , workspaceState :: WorkspaceState
            , windowIds :: [X11Window]
            } deriving (Show, Eq)

class WorkspaceWidgetController wc where
  updateWidget :: wc -> Workspace -> IO wc
  getWidget :: wc -> Gtk.Widget

data WWC =
  forall a. WorkspaceWidgetController a =>
            WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) workspace =
    WWC <$> updateWidget wc workspace

data WorkspaceContentsController = WorkspaceLabelController
  { container :: Gtk.HBox
  , label :: Gtk.Label
  , images :: [Gtk.Image]
  , contentsWorkspace :: Workspace
  }

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget cc = Gtk.toWidget $ container cc
  updateWidget cc newWorkspace = do
    let currentWorkspace = contentsWorkspace cc

    when ((workspaceName currentWorkspace) /= (workspaceName newWorkspace)) $
         Gtk.labelSetMarkup (label cc) (workspaceName newWorkspace)

    newImages <-
      if ((windowIds currentWorkspace) /= (windowIds newWorkspace))
      then
        updateImages cc newWorkspace
      else
        return $ images cc

    return cc { contentsWorkspace = newWorkspace
              , images = newImages
              }

updateImages :: WorkspaceContentsController -> Workspace -> IO [Gtk.Image]
updateImages wcc ws = do
  return $ images wcc

data WorkspaceHUDConfig =
  WorkspaceHUDConfig
  { widgetBuilder :: WorkspaceHUDConfig -> Workspace -> IO WWC
  , widgetGap :: Int
  }

getWorkspaceToWindows :: IO (MM.MultiMap WorkspaceIdx X11Window)
getWorkspaceToWindows =
  withDefaultCtx getWindows >>=
  foldM
    (\theMap window ->
       MM.insert <$> withDefaultCtx (getWorkspace window)
                 <*> pure window <*> pure theMap)
    MM.empty

buildWorkspaces :: IO (M.Map WorkspaceIdx Workspace)
buildWorkspaces = do
  names <- withDefaultCtx getWorkspaceNames
  workspaceToWindows <- getWorkspaceToWindows
  active:visible <- withDefaultCtx getVisibleWorkspaces

  let getWorkspaceState idx windows
        | idx == active = Active
        | elem idx visible = Visible
        | null windows = Empty
        | otherwise = Hidden

  return $ foldl (\theMap (idx, name) ->
                    let windows = MM.lookup idx workspaceToWindows in
                    M.insert idx
                     Workspace { workspaceIdx = idx 
                              , workspaceName = name
                               , workspaceState = getWorkspaceState idx windows
                               , windowIds = windows
                               } theMap) M.empty names

buildWorkspaceWidgets
  :: WorkspaceHUDConfig
  -> Pager
  -> Gtk.HBox
  -> MV.MVar (M.Map WorkspaceIdx WWC)
  -> IO ()
buildWorkspaceWidgets cfg pager container controllersRef = do
  workspacesMap <- buildWorkspaces
  let builder = (widgetBuilder cfg)
      workspaces = M.elems workspacesMap

  workspaceIDToController <-
    M.fromList <$>
    mapM (((liftM2 . liftM2) (,)) (return . workspaceIdx) $ builder cfg) workspaces

  MV.modifyMVar_ controllersRef $ const (return workspaceIDToController)

  mapM_ (Gtk.containerAdd container . getWidget) $ M.elems workspaceIDToController

buildWorkspaceHUD :: WorkspaceHUDConfig -> Pager -> IO Gtk.Widget
buildWorkspaceHUD cfg pager = do
  container <- Gtk.hBoxNew False (widgetGap cfg)
  controllersRef <- MV.newMVar M.empty
  buildWorkspaceWidgets cfg pager container controllersRef
  subscribe pager (onActiveChanged controllersRef) "_NET_CURRENT_DESKTOP"
  subscribe pager (onActiveChanged controllersRef) "_NET_WM_DESKTOP"
  subscribe pager (onActiveChanged controllersRef) "_NET_DESKTOP_NAMES"
  return $ Gtk.toWidget container

  -- let cfg = config pager
  --     activecb = activeCallback cfg deskRef
  --     activefastcb = activeFastCallback cfg deskRef
  --     redrawcb = redrawCallback pager deskRef switcher
  --     urgentcb = urgentCallback cfg deskRef
  -- subscribe pager activecb "_NET_CURRENT_DESKTOP"
  -- subscribe pager activefastcb "_NET_WM_DESKTOP"
  -- subscribe pager redrawcb "_NET_DESKTOP_NAMES"
  -- subscribe pager redrawcb "_NET_NUMBER_OF_DESKTOPS"
  -- subscribe pager urgentcb "WM_HINTS"

updateAllWorkspaceWidgets :: MV.MVar (M.Map WorkspaceIdx WWC) -> IO ()
updateAllWorkspaceWidgets controllersRef = do
  workspacesMap <- buildWorkspaces
  let updateController idx controller =
        maybe (return controller) (updateWidget controller) $
        M.lookup idx workspacesMap

  MV.modifyMVar_ controllersRef $ \controllers -> do
    controllersList <-
      mapM
      (\(idx, controller) -> do
         newController <- (updateController idx controller)
         return (idx, newController)) $
      M.toList controllers
    return $ M.fromList controllersList
  return ()

onActiveChanged :: MV.MVar (M.Map WorkspaceIdx WWC) -> Event -> IO ()
onActiveChanged controllersRef _ =
  Gtk.postGUIAsync $ updateAllWorkspaceWidgets controllersRef

data WorkspaceButtonController =
  WorkspaceButtonController { button :: Gtk.EventBox
                            , buttonWorkspace :: Workspace
                            , contentsController :: WWC
                            }

instance WorkspaceWidgetController WorkspaceButtonController
  where
    getWidget wbc = Gtk.toWidget $ button wbc
    updateWidget wbc workspace = do
      newContents <- updateWidget (contentsController wbc) workspace
      return wbc { contentsController = newContents }

buildButtonController
  :: WorkspaceHUDConfig
  -> Workspace
  -> (WorkspaceHUDConfig -> Workspace -> IO WWC)
  -> IO WWC
buildButtonController cfg workspace contentsBuilder = do
  ebox <- Gtk.eventBoxNew
  cc <- contentsBuilder cfg workspace
  Gtk.containerAdd ebox $ getWidget cc
  return $ WWC WorkspaceButtonController { button = ebox
                                     , buttonWorkspace = workspace
                                     , contentsController = cc
                                     }

data UnderlineController =
  UnderlineController { table :: T.Table
                      -- XXX: An event box is used here because we need to
                      -- change the background
                      , underline :: Gtk.EventBox
                      , overlineController :: WWC
                      }

instance WorkspaceWidgetController UnderlineController
  where
    getWidget uc = Gtk.toWidget $ table uc
    updateWidget uc workspace = do
      Gtk.widgetSetName (underline uc) $ getWidgetName workspace "underline"
      newContents <- updateWidget (overlineController uc) workspace
      return uc { overlineController = newContents }

getWidgetName :: Workspace -> String -> String
getWidgetName ws wname =
  printf "Workspace-%s-%s-%s" wname (workspaceName ws) (map S.toLower $ show $ workspaceState ws)
