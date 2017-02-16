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
  buildButtonController,
  buildContentsController,
  buildUnderlineButtonController,
  buildUnderlineController,
  buildWorkspaceHUD,
  buildWorkspaceWidgets,
  buildWorkspaces,
  defaultWorkspaceHUDConfig,
  getWorkspaceToWindows
) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Char as S
import qualified Data.Map as M
import qualified Data.MultiMap as MM
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Abstract.Widget as W
import qualified Graphics.UI.Gtk.Layout.Table as T
import           Graphics.X11.Xlib.Extras
import           System.Information.EWMHDesktopInfo
import           System.Taffybar.IconImages
import           System.Taffybar.Pager
import           Text.Printf

data WorkspaceState
  = Active
  | Visible
  | Hidden
  | Empty
  | Urgent
  deriving (Show, Eq)

data IconInfo = IIEWMH EWMHIcon | IIFilePath FilePath | IINone

data Workspace =
  Workspace { workspaceIdx :: WorkspaceIdx
            , workspaceName :: String
            , workspaceState :: WorkspaceState
            , windowIds :: [X11Window]
            } deriving (Show, Eq)

class WorkspaceWidgetController wc where
  updateWidget :: wc -> Workspace -> IO wc
  getWidget :: wc -> Gtk.Widget

data WWC = forall a. WorkspaceWidgetController a => WWC a

instance WorkspaceWidgetController WWC where
  getWidget (WWC wc) = getWidget wc
  updateWidget (WWC wc) workspace =
    WWC <$> updateWidget wc workspace

data WorkspaceHUDConfig =
  WorkspaceHUDConfig
  { widgetBuilder :: WorkspaceHUDConfig -> Workspace -> IO WWC
  , widgetGap :: Int
  , windowIconSize :: Int
  , underlineHeight :: Int
  , minWSWidgetSize :: Maybe Int
  , underlinePadding :: Int
  , maxIcons :: Maybe Int
  }

defaultWorkspaceHUDConfig :: WorkspaceHUDConfig
defaultWorkspaceHUDConfig =
  WorkspaceHUDConfig { widgetBuilder = buildUnderlineButtonController
                     , widgetGap = 0
                     , windowIconSize = 16
                     , underlineHeight = 4
                     , minWSWidgetSize = Just 30
                     , underlinePadding = 1
                     , maxIcons = Nothing
                     }

data Context =
  Context { controllersVar :: MV.MVar (M.Map WorkspaceIdx WWC)
          , workspacesVar :: MV.MVar (M.Map WorkspaceIdx Workspace)
          }

updateVar :: MV.MVar a -> (a -> IO a) -> IO a
updateVar var modify = MV.modifyMVar var $ fmap (\a -> (a, a)) . modify

updateWorkspacesVar :: MV.MVar (M.Map WorkspaceIdx Workspace)
                    -> IO (M.Map WorkspaceIdx Workspace)
updateWorkspacesVar workspacesRef = updateVar workspacesRef buildWorkspaces

getWorkspaceToWindows :: IO (MM.MultiMap WorkspaceIdx X11Window)
getWorkspaceToWindows =
  withDefaultCtx getWindows >>=
  foldM
    (\theMap window ->
       MM.insert <$> withDefaultCtx (getWorkspace window)
                 <*> pure window <*> pure theMap)
    MM.empty

buildWorkspaces :: M.Map WorkspaceIdx Workspace -> IO (M.Map WorkspaceIdx Workspace)
buildWorkspaces currentWorkspaces = do
  names <- withDefaultCtx getWorkspaceNames
  workspaceToWindows <- getWorkspaceToWindows
  active:visible <- withDefaultCtx getVisibleWorkspaces

  let
    isCurrentlyUrgent idx =
      maybe False ((== Urgent) . workspaceState) $
            M.lookup idx currentWorkspaces
    getWorkspaceState idx windows
        | idx == active = Active
        | isCurrentlyUrgent idx = Urgent
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
  -> Gtk.HBox
  -> Context
  -> IO ()
buildWorkspaceWidgets cfg cont
                      Context { controllersVar = controllersRef
                              , workspacesVar = workspacesRef
                              } = do
  workspacesMap <- updateWorkspacesVar workspacesRef
  let builder = (widgetBuilder cfg)
      workspaces = M.elems workspacesMap

  workspaceIDToController <-
    M.fromList <$>
    mapM (((liftM2 . liftM2) (,)) (return . workspaceIdx) $ builder cfg) workspaces

  MV.modifyMVar_ controllersRef $ const (return workspaceIDToController)

  mapM_ addWidget $ M.elems workspaceIDToController
  -- XXX: Does this belong somewhere else
  Gtk.widgetShowAll cont
    where addWidget controller =
            do
              let widget = getWidget controller
              Gtk.containerAdd cont widget
              Gtk.boxPackStart cont widget Gtk.PackNatural 0

buildWorkspaceHUD :: WorkspaceHUDConfig -> Pager -> IO Gtk.Widget
buildWorkspaceHUD cfg pager = do
  cont <- Gtk.hBoxNew False (widgetGap cfg)
  controllersRef <- MV.newMVar M.empty
  workspacesRef <- MV.newMVar M.empty
  let context = Context { controllersVar = controllersRef
                        , workspacesVar = workspacesRef
                        }
  buildWorkspaceWidgets cfg cont context

  mapM_ (subscribe pager (onActiveChanged context))
        [ "_NET_CURRENT_DESKTOP"
        , "_NET_WM_DESKTOP"
        , "_NET_DESKTOP_NAMES"
        ]

  return $ Gtk.toWidget cont

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

updateAllWorkspaceWidgets :: Context -> IO ()
updateAllWorkspaceWidgets Context { controllersVar = controllersRef
                                  , workspacesVar = workspacesRef
                                  } = do
  workspacesMap <- updateWorkspacesVar workspacesRef

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

onActiveChanged :: Context -> Event -> IO ()
onActiveChanged context _ =
  Gtk.postGUIAsync $ updateAllWorkspaceWidgets context



data WorkspaceContentsController = WorkspaceContentsController
  { container :: Gtk.HBox
  , label :: Gtk.Label
  , iconImages :: [Gtk.Image]
  , contentsWorkspace :: Workspace
  , contentsConfig :: WorkspaceHUDConfig
  }

buildContentsController :: WorkspaceHUDConfig -> Workspace -> IO WWC
buildContentsController cfg ws = do
  lbl <- Gtk.labelNew (Nothing :: Maybe String)
  hbox <- Gtk.hBoxNew False 0
  Gtk.containerAdd hbox lbl
  let tempController =
        WorkspaceContentsController { container = hbox
                                    , label = lbl
                                    , iconImages = []
                                    , contentsWorkspace =
                                      ws { windowIds = []
                                         , workspaceName = workspaceName ws ++ "fake"
                                         }
                                    , contentsConfig = cfg
                                    }
  WWC <$> updateWidget tempController ws

instance WorkspaceWidgetController WorkspaceContentsController where
  getWidget cc = Gtk.toWidget $ container cc
  updateWidget cc newWorkspace = do
    let currentWorkspace = contentsWorkspace cc
        cfg = contentsConfig cc

    when ((workspaceName currentWorkspace) /= (workspaceName newWorkspace)) $
         Gtk.labelSetMarkup (label cc) (workspaceName newWorkspace)

    newImages <-
      if ((windowIds currentWorkspace) /= (windowIds newWorkspace))
      then
        updateImages cc newWorkspace
      else
        return $ iconImages cc

    Gtk.widgetSetName (container cc) $ getWidgetName newWorkspace "contents"

    maybe (return ()) (updateMinSize $ Gtk.toWidget $ container cc) $
          minWSWidgetSize cfg

    return cc { contentsWorkspace = newWorkspace
              , iconImages = newImages
              }

updateMinSize :: Gtk.Widget -> Int  -> IO ()
updateMinSize widget minWidth = do
  W.widgetSetSizeRequest widget (-1) (-1)
  W.Requisition w _ <- W.widgetSizeRequest widget
  when (w < minWidth) $ W.widgetSetSizeRequest widget minWidth  $ -1

getIconInfo :: WorkspaceHUDConfig -> X11Window -> IO IconInfo
getIconInfo cfg w = do
  -- TODO: handle custom files
  icons <- withDefaultCtx $ getWindowIcons w
  return $ if (null icons)
           then IINone
           else IIEWMH $ selectEWMHIcon (windowIconSize cfg) icons

updateImages :: WorkspaceContentsController -> Workspace -> IO [Gtk.Image]
updateImages wcc ws = do
  iconInfos_ <- mapM (getIconInfo (contentsConfig wcc)) $ windowIds ws
  -- XXX: Only one of the two things being zipped can be an infinite list, which
  -- is why this newImagesNeeded contortion is needed.
  let iconInfos =
        if newImagesNeeded
          then iconInfos_
          else (iconInfos_ ++ repeat IINone)

  newImgs <- zipWithM setImageFromIO getImgs iconInfos
  when newImagesNeeded $ Gtk.widgetShowAll $ container wcc
  return newImgs

  where
    imgSize = windowIconSize $ contentsConfig wcc
    preferCustom = False
    setImageFromIO getImage iconInfo = do
      img <- getImage
      setImage imgSize preferCustom img iconInfo
      return img
    infiniteImages =
      (map return $ iconImages wcc) ++
      (repeat $ do
         img <- Gtk.imageNew
         Gtk.containerAdd (container wcc) img
         return img)
    newImagesNeeded = (length $ iconImages wcc) < (length $ windowIds ws)
    imgSrcs =
      if newImagesNeeded
        then infiniteImages
        else (map return $ iconImages wcc)
    getImgs = case maxIcons $ contentsConfig wcc of
                Just theMax -> take theMax imgSrcs
                Nothing -> imgSrcs

-- | Sets an image based on the image choice (EWMHIcon, custom file, and fill color).
setImage :: Int -> Bool -> Gtk.Image -> IconInfo -> IO ()
setImage imgSize preferCustom img imgChoice =
  case getPixBuf imgSize preferCustom imgChoice of
    Just getPixbuf -> do
      pixbuf <- getPixbuf
      scaledPixbuf <- scalePixbuf imgSize pixbuf
      Gtk.imageSetFromPixbuf img scaledPixbuf
    Nothing -> Gtk.imageClear img

-- | Get the appropriate im\age given an ImageChoice value
getPixBuf :: Int -> Bool -> IconInfo -> Maybe (IO Gtk.Pixbuf)
getPixBuf imgSize preferCustom imgChoice = gpb imgChoice preferCustom
  where gpb (IIFilePath file) True = Just $ pixBufFromFile imgSize file
        gpb (IIEWMH icon) _ = Just $ pixBufFromEWMHIcon icon
        gpb (IIFilePath file) _ = Just $ pixBufFromFile imgSize file
        gpb _ _ = Nothing

data WorkspaceButtonController =
  WorkspaceButtonController { button :: Gtk.EventBox
                            , buttonWorkspace :: Workspace
                            , contentsController :: WWC
                            }

buildButtonController
  :: (WorkspaceHUDConfig -> Workspace -> IO WWC)
  -> WorkspaceHUDConfig
  -> Workspace
  -> IO WWC
buildButtonController contentsBuilder cfg workspace = do
  ebox <- Gtk.eventBoxNew
  cc <- contentsBuilder cfg workspace
  Gtk.containerAdd ebox $ getWidget cc
  _ <- Gtk.on ebox Gtk.buttonPressEvent $ switch $ workspaceIdx workspace
  return $ WWC WorkspaceButtonController { button = ebox
                                         , buttonWorkspace = workspace
                                         , contentsController = cc
                                         }

switch :: (MonadIO m) => WorkspaceIdx -> m Bool
switch idx = do
  liftIO $ withDefaultCtx (switchToWorkspace idx)
  return True

instance WorkspaceWidgetController WorkspaceButtonController
  where
    getWidget wbc = Gtk.toWidget $ button wbc
    updateWidget wbc workspace = do
      newContents <- updateWidget (contentsController wbc) workspace
      return wbc { contentsController = newContents }

data UnderlineController =
  UnderlineController { table :: T.Table
                      -- XXX: An event box is used here because we need to
                      -- change the background
                      , underline :: Gtk.EventBox
                      , overlineController :: WWC
                      }

buildUnderlineController
  :: (WorkspaceHUDConfig -> Workspace -> IO WWC)
  -> WorkspaceHUDConfig
  -> Workspace
  -> IO WWC
buildUnderlineController contentsBuilder cfg workspace = do
  t <- T.tableNew 2 1 False
  u <- Gtk.eventBoxNew
  cc <- contentsBuilder cfg workspace

  W.widgetSetSizeRequest u (-1) $ underlineHeight cfg

  T.tableAttach t (getWidget cc) 0 1 0 1 [T.Expand] [T.Expand] 0 0
  T.tableAttach t u 0 1 1 2 [T.Fill] [T.Shrink] (underlinePadding cfg) 0

  return $ WWC UnderlineController { table = t
                                   , underline = u
                                   , overlineController = cc
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
  printf
    "Workspace-%s-%s-%s"
    wname
    (workspaceName ws)
    (map S.toLower $ show $ workspaceState ws)

buildUnderlineButtonController :: WorkspaceHUDConfig -> Workspace -> IO WWC
buildUnderlineButtonController =
  buildButtonController (buildUnderlineController buildContentsController)

-- TODO:
-- * Handle urgent
-- * Custom Files/Colors
-- * Handle redraw for new workspace
-- * scoll on button for switch
