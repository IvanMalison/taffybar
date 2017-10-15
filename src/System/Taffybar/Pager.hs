-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Pager
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Common support for pager widgets. This module does not provide itself
-- any widgets, but implements an event dispatcher on which widgets can
-- subscribe the desktop events they're interested in, as well as common
-- configuration facilities.
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-- You need only one Pager component to instantiate any number of pager
-- widgets:
--
-- > pager <- pagerNew defaultPagerConfig
-- >
-- > let wss = wspaceSwitcherNew pager  -- Workspace Switcher widget
-- >     los = layoutSwitcherNew pager  -- Layout Switcher widget
-- >     wnd = windowSwitcherNew pager  -- Window Switcher widget
--
-----------------------------------------------------------------------------

module System.Taffybar.Pager
  ( Pager (..)
  , PagerConfig (..)
  , PagerIO
  , defaultPagerConfig
  , pagerNew
  , subscribe
  , colorize
  , liftPagerX11
  , liftPagerX11Def
  , runWithPager
  , shorten
  , wrap
  , escape
  ) where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Exception.Enclosed (catchAny)
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map as M
import Graphics.UI.Gtk (escapeMarkup)
import Graphics.X11.Types
import Graphics.X11.Xlib.Extras
  hiding (rawGetWindowProperty, getWindowProperty8,
          getWindowProperty16, getWindowProperty32)
import System.Information.EWMHDesktopInfo
import System.Information.X11DesktopInfo
import Text.Printf (printf)

type Listener = Event -> IO ()
type Filter = Atom
type SubscriptionList = IORef [(Listener, Filter)]

-- | Structure contanining functions to customize the pretty printing of
-- different widget elements.
data PagerConfig = PagerConfig
  -- | the name of the active window.
  { activeWindow            :: String -> String
  -- | the currently active layout.
  , activeLayout            :: String -> String
  -- | the currently active workspace.
  , activeWorkspace         :: String -> String
  -- | inactive workspace with windows.
  , hiddenWorkspace         :: String -> String
  -- | inactive workspace with no windows.
  , emptyWorkspace          :: String -> String
  -- | all other visible workspaces (Xinerama or XRandR).
  , visibleWorkspace        :: String -> String
  -- | workspaces containing windows with the urgency hint set.
  , urgentWorkspace         :: String -> String
  -- | separator to use between desktop widgets in 'TaffyPager'.
  , widgetSep               :: String
  -- | wrap workspace buttons in a frame
  , workspaceBorder         :: Bool
  -- | space in pixels between workspace buttons
  , workspaceGap            :: Int
  -- | pad workspace name in button
  , workspacePad            :: Bool
  -- | use images in the workspace switcher
  , useImages               :: Bool
  -- | image height and width in pixels
  , imageSize               :: Int
  -- | fill empty images instead of clearing them
  , fillEmptyImages         :: Bool
  -- | use custom icons over EWHMIcons
  , preferCustomIcon        :: Bool
  -- | get icon based on window title and class
  , customIcon              :: String -> String -> Maybe FilePath
  -- | title windows for WindowSwitcher
  , windowSwitcherFormatter :: M.Map WorkspaceIdx String -> X11WindowHandle -> String
  }

-- | Structure containing the state of the Pager.
data Pager = Pager
  { config  :: PagerConfig -- ^ the configuration settings.
  , clients :: SubscriptionList -- ^ functions to apply on incoming events depending on their types.
  , pagerX11ContextVar :: IORef X11Context
  }

type PagerIO a = ReaderT Pager IO a

liftPagerX11 :: X11Property a -> PagerIO a
liftPagerX11 prop = ask >>= lift . flip runWithPager prop

liftPagerX11Def :: a -> X11Property a -> PagerIO a
liftPagerX11Def def prop = liftPagerX11 $ postX11RequestSyncProp prop def

runWithPager :: Pager -> X11Property a -> IO a
runWithPager pager prop = do
  x11Ctx <- readIORef $ pagerX11ContextVar pager
  -- runWithPager should probably changed so that it takes a default value
  runReaderT prop x11Ctx

-- | Default pretty printing options.
defaultPagerConfig :: PagerConfig
defaultPagerConfig = PagerConfig
  { activeWindow            = escape . shorten 40
  , activeLayout            = escape
  , activeWorkspace         = colorize "yellow" "" . wrap "[" "]" . escape
  , hiddenWorkspace         = escape
  , emptyWorkspace          = const ""
  , visibleWorkspace        = wrap "(" ")" . escape
  , urgentWorkspace         = colorize "red" "yellow" . escape
  , widgetSep               = " : "
  , workspaceBorder         = False
  , workspaceGap            = 0
  , workspacePad            = True
  , useImages               = False
  , imageSize               = 16
  , fillEmptyImages         = False
  , preferCustomIcon        = False
  , customIcon              = \_ _ -> Nothing
  , windowSwitcherFormatter = defaultFormatEntry
  }

-- | Build the name to display in the list of windows by prepending the name
-- of the workspace it is currently in to the name of the window itself
defaultFormatEntry
  :: M.Map WorkspaceIdx String -- ^ List $ names of all available workspaces
  -> X11WindowHandle -- ^ Handle of the window to name
  -> String
defaultFormatEntry wsNames ((ws, wtitle, _), _) =
  printf "%s: %s " wsName $ nonEmpty wtitle
  where
    wsName = M.findWithDefault ("WS#" ++ show wsN) ws wsNames
    WSIdx wsN = ws
    nonEmpty x =
      case x of
        [] -> "(nameless window)"
        _ -> x

-- | Creates a new Pager component (wrapped in the IO Monad) that can be
-- used by widgets for subscribing X11 events.
pagerNew :: PagerConfig -> IO Pager
pagerNew cfg = do
  ref <- newIORef []
  ctx <- getDefaultCtx
  ctxVar <- newIORef ctx
  let pager = Pager cfg ref ctxVar
  _ <- forkIO $ withDefaultCtx (eventLoop $ handleEvent ref)
  return pager
    where handleEvent :: SubscriptionList -> Event -> IO ()
          handleEvent ref event = do
            listeners <- readIORef ref
            mapM_ (notify event) listeners

-- | Passes the given Event to the given Listener, but only if it was
-- registered for that type of events via 'subscribe'.
notify :: Event -> (Listener, Filter) -> IO ()
notify event (listener, eventFilter) =
  case event of
    PropertyEvent _ _ _ _ _ atom _ _ ->
      when (atom == eventFilter) $ catchAny (listener event) ignoreException
    _ -> return ()

-- | Registers the given Listener as a subscriber of events of the given
-- type: whenever a new event of the type with the given name arrives to
-- the Pager, it will execute Listener on it.
subscribe :: Pager -> Listener -> String -> IO ()
subscribe pager listener filterName = do
  eventFilter <- runWithPager pager $ getAtom filterName
  registered <- readIORef (clients pager)
  let next = (listener, eventFilter)
  writeIORef (clients pager) (next : registered)

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

-- | Creates markup with the given foreground and background colors and the
-- given contents.
colorize :: String -- ^ Foreground color.
         -> String -- ^ Background color.
         -> String -- ^ Contents.
         -> String
colorize fg bg = printf "<span%s%s>%s</span>" (attr "fg" fg) (attr "bg" bg)
  where attr name value
          | null value = ""
          | otherwise  = printf " %scolor=\"%s\"" name value

-- | Limit a string to a certain length, adding "..." if truncated.
shorten :: Int -> String -> String
shorten l s
  | length s <= l = s
  | l >= 3        = take (l - 3) s ++ "..."
  | otherwise     = "..."

-- | Wrap the given string in the given delimiters.
wrap :: String -- ^ Left delimiter.
     -> String -- ^ Right delimiter.
     -> String -- ^ Output string.
     -> String
wrap open close s = open ++ s ++ close

-- | Escape strings so that they can be safely displayed by Pango in the
-- bar widget
escape :: String -> String
escape = escapeMarkup
