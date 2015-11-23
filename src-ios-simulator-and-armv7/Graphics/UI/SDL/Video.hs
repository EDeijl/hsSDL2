{-# LINE 1 "Graphics/UI/SDL/Video.hsc" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LINE 2 "Graphics/UI/SDL/Video.hsc" #-}

{-# LINE 3 "Graphics/UI/SDL/Video.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.SDL.Video
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------
module Graphics.UI.SDL.Video
  ( -- * Window management
    withWindow
  , createWindow
  , createWindowFrom
  , destroyWindow

  , showWindow
  , hideWindow
  , maximizeWindow
  , minimizeWindow
  , raiseWindow
  , restoreWindow

  , setWindowBrightness
  , getWindowBrightness

  , setWindowGrab
  , getWindowGrab
  , setWindowIcon

  , setWindowMaximumSize
  , getWindowMaximumSize

  , setWindowMinimumSize
  , getWindowMinimumSize

  , setWindowPosition
  , getWindowPosition

  , setWindowSize
  , getWindowSize

  , setWindowTitle
  , getWindowTitle

  , getWindowPixelFormat

  , setWindowDisplayMode
  , getWindowDisplayMode

  , getWindowDisplayIndex

  , WindowID
  , getWindowID
  , getWindowFromID

    -- * OpenGL
  , GLAttribute (..)
  , withBoundTexture
  , withOpenGL
  , glBindTexture
  , glCreateContext
  , glDeleteContext
  , glExtensionSupported
  , glGetCurrentContext
  , glGetCurrentWindow
  , glGetDrawableSize
  , glSwapWindow
  , glUnbindTexture
  , glGetAttribute
  , glResetAttributes
  , glSetAttribute
  , glSetSwapInterval
  , glGetSwapInterval
    -- * some more constants
  , glContextProfileCore
  , glContextProfileCompatibility
  , glContextProfileES


  , SwapInterval(..)

    -- * Surfaces
  , surfaceFormat

    -- * Screensaver handling
  , disableScreenSaver
  , enableScreenSaver
  , withoutScreenSaver
  , isScreenSaverEnabled

    -- * Pixel formats
  , allocFormat
  , mapRGB
  , mapRGBA

    -- * Display Modes
  , DisplayMode(..)
  , getClosestDisplayMode
  , getCurrentDisplayMode
  , getDesktopDisplayMode
  , getDisplayMode

  , getDisplayName
  , getNumDisplayModes
  , getNumVideoDisplays
  , getCurrentVideoDriver
  , getDisplayBounds
  , getNumVideoDrivers
  , getVideoDriver
  , getWindowFlags

  , videoInit
  , videoQuit
  ) where

import Control.Applicative
import Control.Exception (bracket, bracket_)
import Control.Monad
import Data.ByteString (useAsCString)
import Data.Text.Encoding
import Foreign hiding (void)
import Foreign.C
import Foreign.C.Types

import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Utilities
import Graphics.UI.SDL.General
import Graphics.UI.SDL.Rect (Rect(..))
import Graphics.UI.SDL.Raw

import qualified Data.Text as T

type WindowID = Int

--------------------------------------------------------------------------------
-- XXX: Will SDL2 always copy the given cstring?
withUtf8CString :: String -> (CString -> IO a) -> IO a
withUtf8CString = useAsCString . encodeUtf8 . T.pack

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_CreateWindow"
  sdlCreateWindow :: CString -> CInt -> CInt -> CInt -> CInt -> Word32 -> IO (Ptr WindowStruct)
{-# LINE 148 "Graphics/UI/SDL/Video.hsc" #-}

createWindow :: String -> Position -> Size -> [WindowFlag] -> IO Window
createWindow title (Position x y) (Size w h) flags =
  withUtf8CString title $ \cstr -> do
    window <- fatalSDLNull "SDL_CreateWindow" $
      sdlCreateWindow
        cstr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)
             (toBitmask windowFlagToC flags)
    mkFinalizedWindow window

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_CreateWindowFrom"
  sdlCreateWindowFrom :: Ptr a -> IO (Ptr WindowStruct)

createWindowFrom :: Ptr a -> IO Window
createWindowFrom = fatalSDLNull "SDL_CreateWindowFrom" . sdlCreateWindowFrom >=> mkFinalizedWindow

--------------------------------------------------------------------------------
withWindow :: String -> Position -> Size -> [WindowFlag] -> (Window -> IO r) -> IO r
withWindow title position size flags =
  bracket (createWindow title position size flags) destroyWindow

destroyWindow :: Window -> IO ()
destroyWindow = finalizeForeignPtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_CreateContext"
  sdlGlCreateContext :: Ptr WindowStruct -> IO (Ptr GLContextStruct)

glCreateContext :: Window -> IO GLContext
glCreateContext w = withForeignPtr w $
  fatalSDLNull "SDL_GL_CreateContext" . sdlGlCreateContext >=>
    touchForeignPtr sdlGlDeleteContext_finalizer

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_GetCurrentContext"
  sdlGlGetCurrentContext :: IO (Ptr GLContextStruct)

glGetCurrentContext :: IO GLContext
glGetCurrentContext =
  fatalSDLNull "SDL_GL_GetCurrentContext" sdlGlGetCurrentContext >>= newForeignPtr_

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_GetCurrentWindow"
  sdlGlGetCurrentWindow :: IO (Ptr WindowStruct)

glGetCurrentWindow :: IO Window
glGetCurrentWindow =
  fatalSDLNull "SDL_GL_GetCurrentWindow" sdlGlGetCurrentWindow >>= newForeignPtr_

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_GetDrawableSize"
  sdlGlGetDrawableSize :: Ptr WindowStruct -> Ptr Int32 -> Ptr Int32 -> IO ()
{-# LINE 201 "Graphics/UI/SDL/Video.hsc" #-}

glGetDrawableSize :: Window -> IO Size
glGetDrawableSize window = withForeignPtr window $ \cWin ->
  alloca $ \wPtr -> alloca $ \hPtr -> do
    sdlGlGetDrawableSize cWin wPtr hPtr
    Size <$> (fromIntegral <$> peek wPtr) <*> (fromIntegral <$> peek hPtr)

--------------------------------------------------------------------------------
foreign import ccall safe "&SDL_GL_DeleteContext"
  sdlGlDeleteContext_finalizer :: FunPtr (Ptr GLContextStruct -> IO ())

glDeleteContext :: GLContext -> IO ()
glDeleteContext = finalizeForeignPtr

--------------------------------------------------------------------------------
withOpenGL :: Window -> IO a -> IO a
withOpenGL win = bracket (glCreateContext win) glDeleteContext . const

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_ExtensionSupported"
  sdlGlExtensionSupported :: CString -> IO Word32
{-# LINE 222 "Graphics/UI/SDL/Video.hsc" #-}

glExtensionSupported :: String -> IO Bool
glExtensionSupported ext = withCString ext $
  fmap sdlBoolToBool . sdlGlExtensionSupported

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_SwapWindow"
  sdlGlSwapWindow :: Ptr WindowStruct -> IO ()

glSwapWindow :: Window -> IO ()
glSwapWindow w = withForeignPtr w sdlGlSwapWindow

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_BindTexture"
  sdlGlBindTexture :: Ptr TextureStruct -> Ptr CFloat -> Ptr CFloat -> IO Int32
{-# LINE 237 "Graphics/UI/SDL/Video.hsc" #-}

-- | Bind a texture to the active texture unit in the current OpenGL context.
glBindTexture :: Texture -> IO ()
glBindTexture tex = void $ withForeignPtr tex $ \texp ->
  fatalSDLBool "SDL_GL_BindTexture" $ sdlGlBindTexture texp nullPtr nullPtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_UnbindTexture"
  sdlGlUnbindTexture :: Ptr TextureStruct -> IO CInt

-- | Unbind a texture from the current OpenGL context.
glUnbindTexture :: Texture -> IO ()
glUnbindTexture tex = Control.Monad.void $ withForeignPtr tex $ \texp ->
  sdlGlUnbindTexture texp

-- | Run an action with a texture bound to the active texture unit in the current OpenGL context, and unbind it afterwards.
withBoundTexture :: Texture -> IO a -> IO a
withBoundTexture tex = bracket_ (glBindTexture tex) (glUnbindTexture tex)

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_GetAttribute"
  sdlGlGetAttribute :: Int32 -> Ptr Int32 -> IO Int32
{-# LINE 259 "Graphics/UI/SDL/Video.hsc" #-}

data GLAttribute
  = GLRedSize
  | GLGreenSize
  | GLBlueSize
  | GLAlphaSize
  | GLBufferSize
  | GLDoubleBuffer
  | GLDepthSize
  | GLStencilSize
  | GLAccumRedSize
  | GLAccumGreenSize
  | GLAccumBlueSize
  | GLAccumAlphaSize
  | GLStereo
  | GLMultiSampleBuffers
  | GLMultiSampleSamples
  | GLAcceleratedVisual
  | GLRetainedBacking
  | GLContextMajorVersion
  | GLContextMinorVersion
  | GLContextFlags
  | GLContextProfileMask
  | GLShareWithCurrentContext
  | GLFramebufferSRGBCapable
  | GLContextEGL

sdlGLAttributeToC :: GLAttribute -> Int32
{-# LINE 287 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLRedSize = 0
{-# LINE 288 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLGreenSize = 0
{-# LINE 289 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLBlueSize = 2
{-# LINE 290 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLAlphaSize = 3
{-# LINE 291 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLBufferSize = 4
{-# LINE 292 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLDoubleBuffer = 5
{-# LINE 293 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLDepthSize = 6
{-# LINE 294 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLStencilSize = 7
{-# LINE 295 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLAccumRedSize = 8
{-# LINE 296 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLAccumGreenSize = 9
{-# LINE 297 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLAccumBlueSize = 10
{-# LINE 298 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLAccumAlphaSize = 11
{-# LINE 299 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLStereo = 12
{-# LINE 300 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLMultiSampleBuffers = 13
{-# LINE 301 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLMultiSampleSamples = 14
{-# LINE 302 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLAcceleratedVisual = 15
{-# LINE 303 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLRetainedBacking = 16
{-# LINE 304 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLContextMajorVersion = 17
{-# LINE 305 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLContextMinorVersion = 18
{-# LINE 306 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLContextFlags = 20
{-# LINE 307 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLContextProfileMask = 21
{-# LINE 308 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLShareWithCurrentContext = 22
{-# LINE 309 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLFramebufferSRGBCapable = 23
{-# LINE 310 "Graphics/UI/SDL/Video.hsc" #-}
sdlGLAttributeToC GLContextEGL = 19
{-# LINE 311 "Graphics/UI/SDL/Video.hsc" #-}

glGetAttribute :: GLAttribute -> IO Int32
{-# LINE 313 "Graphics/UI/SDL/Video.hsc" #-}
glGetAttribute attribute = alloca $ \payloadPtr ->  do
  fatalSDLBool "SDL_GL_GetAttribute" $
    sdlGlGetAttribute (sdlGLAttributeToC attribute) payloadPtr
  peek payloadPtr

--------------------------------------------------------------------------------
glContextProfileCore, glContextProfileCompatibility, glContextProfileES :: Int32
{-# LINE 320 "Graphics/UI/SDL/Video.hsc" #-}
glContextProfileCore = 1
{-# LINE 321 "Graphics/UI/SDL/Video.hsc" #-}
glContextProfileCompatibility = 2
{-# LINE 322 "Graphics/UI/SDL/Video.hsc" #-}
glContextProfileES = 4
{-# LINE 323 "Graphics/UI/SDL/Video.hsc" #-}

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_SetAttribute"
  sdlGlSetAttribute :: Int32 -> Int32 -> IO Int32
{-# LINE 327 "Graphics/UI/SDL/Video.hsc" #-}

glSetAttribute :: GLAttribute -> Int32 -> IO ()
{-# LINE 329 "Graphics/UI/SDL/Video.hsc" #-}
glSetAttribute attribute value = fatalSDLBool "SDL_GL_SetAttribute" $
  sdlGlSetAttribute (sdlGLAttributeToC attribute) value

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_ResetAttributes"
  glResetAttributes :: IO ()

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_SetSwapInterval"
  sdlGlSetSwapInterval :: Int32 -> IO Int32
{-# LINE 339 "Graphics/UI/SDL/Video.hsc" #-}

data SwapInterval = ImmediateUpdates | SynchronizedUpdates | LateSwapTearing

swapIntervalToC :: SwapInterval -> Int32
{-# LINE 343 "Graphics/UI/SDL/Video.hsc" #-}
swapIntervalToC ImmediateUpdates = 0
swapIntervalToC SynchronizedUpdates = 1
swapIntervalToC LateSwapTearing = -1

glSetSwapInterval :: SwapInterval -> IO ()
glSetSwapInterval swapInterval = fatalSDLBool "SDL_GL_SetSwapInterval" $
  sdlGlSetSwapInterval (swapIntervalToC swapInterval)

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GL_SetSwapInterval"
  sdlGlGetSwapInterval :: IO Int32
{-# LINE 354 "Graphics/UI/SDL/Video.hsc" #-}

swapIntervalFromC :: Int32 -> SwapInterval
{-# LINE 356 "Graphics/UI/SDL/Video.hsc" #-}
swapIntervalFromC 0 = ImmediateUpdates
swapIntervalFromC 1 = SynchronizedUpdates
swapIntervalFromC (-1) = LateSwapTearing
swapIntervalFromC unknown = error $ "Graphics.UI.SDL.Video.swapIntervalFromC called with unknown argument: " ++ show unknown

glGetSwapInterval :: IO SwapInterval
glGetSwapInterval = swapIntervalFromC <$> sdlGlGetSwapInterval

--------------------------------------------------------------------------------
-- void SDL_DisableScreenSaver(void)
foreign import ccall safe "SDL_DisableScreenSaver"
  disableScreenSaver :: IO ()

-- void SDL_EnableScreenSaver(void)
foreign import ccall safe "SDL_EnableScreenSaver"
  enableScreenSaver :: IO ()

withoutScreenSaver :: IO a -> IO a
withoutScreenSaver = bracket_ disableScreenSaver enableScreenSaver

-- SDL_bool SDL_IsScreenSaverEnabled(void)
foreign import ccall safe "SDL_IsScreenSaverEnabled"
  sdlIsScreenSaverEnabled :: IO SDL_bool

isScreenSaverEnabled :: IO Bool
isScreenSaverEnabled = fmap (/= 0) sdlIsScreenSaverEnabled

-- void SDL_HideWindow(SDL_Window* window)
foreign import ccall safe "SDL_HideWindow" sdlHideWindow :: Ptr WindowStruct -> IO ()

hideWindow :: Window -> IO ()
hideWindow win = withForeignPtr win sdlHideWindow

-- void SDL_MaximizeWindow(SDL_Window* window)
foreign import ccall safe "SDL_MaximizeWindow" sdlMaximizeWindow :: Ptr WindowStruct -> IO ()

maximizeWindow :: Window -> IO ()
maximizeWindow win = withForeignPtr win sdlMaximizeWindow

-- void SDL_MinimizeWindow(SDL_Window* window)
foreign import ccall safe "SDL_MinimizeWindow" sdlMinimizeWindow :: Ptr WindowStruct -> IO ()

minimizeWindow :: Window -> IO ()
minimizeWindow win = withForeignPtr win sdlMinimizeWindow

-- void SDL_RaiseWindow(SDL_Window* window)
foreign import ccall safe "SDL_RaiseWindow" sdlRaiseWindow :: Ptr WindowStruct -> IO ()

raiseWindow :: Window -> IO ()
raiseWindow win = withForeignPtr win sdlRaiseWindow

-- void SDL_RestoreWindow(SDL_Window* window)
foreign import ccall safe "SDL_RestoreWindow" sdlRestoreWindow :: Ptr WindowStruct -> IO ()

restoreWindow :: Window -> IO ()
restoreWindow win = withForeignPtr win sdlRestoreWindow

-- void SDL_ShowWindow(SDL_Window* window)
foreign import ccall safe "SDL_ShowWindow" sdlShowWindow :: Ptr WindowStruct -> IO ()

showWindow :: Window -> IO ()
showWindow win = withForeignPtr win sdlShowWindow

-- int SDL_SetWindowBrightness(SDL_Window* window, float brightness)
foreign import ccall safe "SDL_SetWindowBrightness"
  sdlSetWindowBrightness :: Ptr WindowStruct -> CFloat -> IO CInt

setWindowBrightness :: Window -> Double -> IO ()
setWindowBrightness win brightness =
  unwrapBool "setWindowBrightness" $
  withForeignPtr win $ \cw ->
    fmap (==0) (sdlSetWindowBrightness cw (realToFrac brightness))

-- float SDL_GetWindowBrightness(SDL_Window* window)
foreign import ccall safe "SDL_GetWindowBrightness"
  sdlGetWindowBrightness :: Ptr WindowStruct -> IO CFloat

-- FIXME: Error handling?
getWindowBrightness :: Window -> IO Double
getWindowBrightness win =
  withForeignPtr win $
  fmap realToFrac . sdlGetWindowBrightness

-- void* SDL_SetWindowData(SDL_Window* window, const char* name, void* userdata)
-- void* SDL_GetWindowData(SDL_Window* window, const char* name)
-- int SDL_SetWindowFullscreen(SDL_Window* window, Uint32 flags)
-- int SDL_SetWindowGammaRamp(SDL_Window*window,const Uint16* red,const Uint16* green,const Uint16* blue)
-- int SDL_GetWindowGammaRamp(SDL_Window* window,Uint16*red,Uint16*green,Uint16*blue)

-- void SDL_SetWindowGrab(SDL_Window* window, SDL_bool    grabbed)
foreign import ccall safe "SDL_SetWindowGrab"
  sdlSetWindowGrab :: Ptr WindowStruct -> SDL_bool -> IO ()

setWindowGrab :: Window -> Bool -> IO ()
setWindowGrab win flag =
  withForeignPtr win $ \cw ->
  sdlSetWindowGrab cw (if flag then 1 else 0)

-- SDL_bool SDL_GetWindowGrab(SDL_Window* window)
foreign import ccall safe "SDL_GetWindowGrab"
  sdlGetWindowGrab :: Ptr WindowStruct -> IO SDL_bool

getWindowGrab :: Window -> IO Bool
getWindowGrab win = withForeignPtr win $ fmap (/=0) . sdlGetWindowGrab

foreign import ccall safe "SDL_SetWindowIcon"
  sdlSetWindowIcon :: Ptr WindowStruct -> Ptr SurfaceStruct -> IO ()

setWindowIcon :: Window -> Surface -> IO ()
setWindowIcon win icon =
  withForeignPtr win $ \cw ->
    withForeignPtr icon $ \icon' -> sdlSetWindowIcon cw icon'

-- void SDL_SetWindowMaximumSize(SDL_Window* window,int max_w,int max_h)
foreign import ccall safe "SDL_SetWindowMaximumSize"
  sdlSetWindowMaximumSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowMaximumSize :: Window -> Size -> IO ()
setWindowMaximumSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowMaximumSize cw (fromIntegral height) (fromIntegral width)

-- void SDL_GetWindowMaximumSize(SDL_Window* window,int*w,int*h)
foreign import ccall safe "SDL_GetWindowMaximumSize"
  sdlGetWindowMaximumSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowMaximumSize :: Window -> IO Size
getWindowMaximumSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowMaximumSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowMinimumSize(SDL_Window* window,int min_w,int min_h)
foreign import ccall safe "SDL_SetWindowMinimumSize"
  sdlSetWindowMinimumSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowMinimumSize :: Window -> Size -> IO ()
setWindowMinimumSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowMinimumSize cw (fromIntegral width) (fromIntegral height)

-- void SDL_GetWindowMinimumSize(SDL_Window* window, int*w, int*h)
foreign import ccall safe "SDL_GetWindowMinimumSize"
  sdlGetWindowMinimumSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowMinimumSize :: Window -> IO Size
getWindowMinimumSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowMinimumSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowPosition(SDL_Window* window, int x, int y)
foreign import ccall safe "SDL_SetWindowPosition"
  sdlSetWindowPosition :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowPosition :: Window -> Position -> IO ()
setWindowPosition win (Position x y) =
  withForeignPtr win $ \cw ->
  sdlSetWindowPosition cw (fromIntegral x) (fromIntegral y)

-- void SDL_GetWindowPosition(SDL_Window* window, int*x, int*y)
foreign import ccall safe "SDL_GetWindowPosition"
  sdlGetWindowPosition :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowPosition :: Window -> IO Position
getWindowPosition win =
  withForeignPtr win $ \cw ->
  alloca $ \xPtr ->
  alloca $ \yPtr -> do
    sdlGetWindowPosition cw xPtr yPtr
    mkPosition <$> peek xPtr <*> peek yPtr

-- void SDL_SetWindowSize(SDL_Window* window, int w, int h)
foreign import ccall safe "SDL_SetWindowSize"
  sdlSetWindowSize :: Ptr WindowStruct -> CInt -> CInt -> IO ()

setWindowSize :: Window -> Size -> IO ()
setWindowSize win (Size width height) =
  withForeignPtr win $ \cw ->
  sdlSetWindowSize cw (fromIntegral width) (fromIntegral height)

-- void SDL_GetWindowSize(SDL_Window* window, int*w, int*h)
foreign import ccall safe "SDL_GetWindowSize"
  sdlGetWindowSize :: Ptr WindowStruct -> Ptr CInt -> Ptr CInt -> IO ()

getWindowSize :: Window -> IO Size
getWindowSize win =
  withForeignPtr win $ \cw ->
  alloca $ \widthPtr ->
  alloca $ \heightPtr -> do
    sdlGetWindowSize cw widthPtr heightPtr
    mkSize <$> peek widthPtr <*> peek heightPtr

-- void SDL_SetWindowTitle(SDL_Window* window, const char* title)
foreign import ccall safe "SDL_SetWindowTitle"
  sdlSetWindowTitle :: Ptr WindowStruct -> CString -> IO ()

setWindowTitle :: Window -> String -> IO ()
setWindowTitle win title =
  withUtf8CString title $ \cstr ->
        withForeignPtr win $ \winptr -> sdlSetWindowTitle winptr cstr

-- const char* SDL_GetWindowTitle(SDL_Window* window)

foreign import ccall safe "SDL_GetWindowTitle"
  sdlGetWindowTitle :: Ptr WindowStruct -> IO CString

getWindowTitle :: Window -> IO String
getWindowTitle w = withForeignPtr w $ sdlGetWindowTitle >=> peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetWindowPixelFormat"
  sdlGetWindowPixelFormat :: Ptr WindowStruct -> IO Word32

foreign import ccall safe "SDL_AllocFormat"
  sdlAllocFormat :: Word32 -> IO (Ptr PixelFormatStruct)

getWindowPixelFormat :: Window -> IO Word32
getWindowPixelFormat w = withForeignPtr w sdlGetWindowPixelFormat

allocFormat :: Word32 -> IO PixelFormat
allocFormat pf = sdlAllocFormat pf >>= newForeignPtr sdlFreeFormat_finalizer

foreign import ccall safe "&SDL_FreeFormat"
  sdlFreeFormat_finalizer :: FunPtr (Ptr PixelFormatStruct -> IO ())

foreign import ccall safe "SDL_MapRGB"
  sdlMapRGB :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> IO Word32

mapRGB :: PixelFormat -> Word8 -> Word8 -> Word8 -> IO Word32
mapRGB p r g b = withForeignPtr p $ \cp -> sdlMapRGB cp r g b

foreign import ccall safe "SDL_MapRGBA"
  sdlMapRGBA :: Ptr PixelFormatStruct -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32

mapRGBA :: PixelFormat -> Word8 -> Word8 -> Word8 -> Word8 -> IO Word32
mapRGBA p r g b a = withForeignPtr p $ \cp -> sdlMapRGBA cp r g b a

surfaceFormat :: Surface -> IO PixelFormat
surfaceFormat s =
  withForeignPtr s $ \cs ->
  (\hsc_ptr -> peekByteOff hsc_ptr 4) cs >>= newForeignPtr_
{-# LINE 602 "Graphics/UI/SDL/Video.hsc" #-}

--------------------------------------------------------------------------------
data DisplayMode = DisplayMode { displayModeFormat :: PixelFormatEnum
                               , displayModeWidth  :: Int32
{-# LINE 606 "Graphics/UI/SDL/Video.hsc" #-}
                               , displayModeHeight :: Int32
{-# LINE 607 "Graphics/UI/SDL/Video.hsc" #-}
                               , displayModeRefreshRate :: Int32
{-# LINE 608 "Graphics/UI/SDL/Video.hsc" #-}
                               } deriving (Eq, Show)

instance Storable DisplayMode where
  sizeOf = const (20)
{-# LINE 612 "Graphics/UI/SDL/Video.hsc" #-}

  alignment = const 4

  poke ptr DisplayMode{..} = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr (pixelFormatEnumToC displayModeFormat)
{-# LINE 617 "Graphics/UI/SDL/Video.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr displayModeWidth
{-# LINE 618 "Graphics/UI/SDL/Video.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 8) ptr displayModeHeight
{-# LINE 619 "Graphics/UI/SDL/Video.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 12) ptr displayModeRefreshRate
{-# LINE 620 "Graphics/UI/SDL/Video.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 16) ptr nullPtr
{-# LINE 621 "Graphics/UI/SDL/Video.hsc" #-}

  peek ptr = DisplayMode
    <$> (pixelFormatEnumFromC <$> (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr)
{-# LINE 624 "Graphics/UI/SDL/Video.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 625 "Graphics/UI/SDL/Video.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 8) ptr
{-# LINE 626 "Graphics/UI/SDL/Video.hsc" #-}
    <*> (\hsc_ptr -> peekByteOff hsc_ptr 12) ptr
{-# LINE 627 "Graphics/UI/SDL/Video.hsc" #-}

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetDisplayMode"
  sdlGetDisplayMode :: Int32 -> Int32 -> Ptr DisplayMode -> IO Int32
{-# LINE 631 "Graphics/UI/SDL/Video.hsc" #-}

getDisplayMode :: Int32 -> Int32 -> IO DisplayMode
{-# LINE 633 "Graphics/UI/SDL/Video.hsc" #-}
getDisplayMode d m = alloca $ \displayModePtr -> do
  fatalSDLBool "SDL_GetDisplayMode" (sdlGetDisplayMode d m displayModePtr)
  peek displayModePtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetCurrentDisplayMode"
  sdlGetCurrentDisplayMode :: Int32 -> Ptr DisplayMode -> IO Int32
{-# LINE 640 "Graphics/UI/SDL/Video.hsc" #-}

getCurrentDisplayMode :: Int32 -> IO DisplayMode
{-# LINE 642 "Graphics/UI/SDL/Video.hsc" #-}
getCurrentDisplayMode d = alloca $ \displayModePtr -> do
  fatalSDLBool "SDL_GetCurrentDisplayMode" (sdlGetCurrentDisplayMode d displayModePtr)
  peek displayModePtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetDesktopDisplayMode"
  sdlGetDesktopDisplayMode :: Int32 -> Ptr DisplayMode -> IO Int32
{-# LINE 649 "Graphics/UI/SDL/Video.hsc" #-}

getDesktopDisplayMode :: Int32 -> IO DisplayMode
{-# LINE 651 "Graphics/UI/SDL/Video.hsc" #-}
getDesktopDisplayMode d = alloca $ \displayModePtr -> do
  fatalSDLBool "SDL_GetDesktopDisplayMode" (sdlGetDesktopDisplayMode d displayModePtr)
  peek displayModePtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetClosestDisplayMode"
  sdlGetClosestDisplayMode :: Int32 -> Ptr DisplayMode -> Ptr DisplayMode -> IO (Ptr DisplayMode)
{-# LINE 658 "Graphics/UI/SDL/Video.hsc" #-}

getClosestDisplayMode :: Int32 -> DisplayMode -> IO (Maybe DisplayMode)
{-# LINE 660 "Graphics/UI/SDL/Video.hsc" #-}
getClosestDisplayMode d mode =
  with mode $ \modePtr ->
  alloca $ \closestPtr -> do
    _ <- sdlGetClosestDisplayMode d modePtr closestPtr
    maybePeek peek closestPtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetWindowDisplayMode"
  sdlGetWindowDisplayMode :: Ptr WindowStruct -> Ptr DisplayMode -> IO Int32
{-# LINE 669 "Graphics/UI/SDL/Video.hsc" #-}

getWindowDisplayMode :: Window -> IO DisplayMode
getWindowDisplayMode win =
  alloca $ \modePtr ->
  withForeignPtr win $ \cw -> do
    fatalSDLBool "SDL_GetWindowDisplayMode" (sdlGetWindowDisplayMode cw modePtr)
    peek modePtr

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_SetWindowDisplayMode"
  sdlSetWindowDisplayMode :: Ptr WindowStruct -> Ptr DisplayMode -> IO Int32
{-# LINE 680 "Graphics/UI/SDL/Video.hsc" #-}

setWindowDisplayMode :: Window -> Maybe DisplayMode -> IO ()
setWindowDisplayMode win mode =
  withForeignPtr win $ \cw ->
  maybeWith with mode $ \modePtr ->
  fatalSDLBool "SDL_SetWindowDisplayMode" (sdlSetWindowDisplayMode cw modePtr)

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetWindowDisplayIndex"
  sdlGetWindowDisplayIndex :: Ptr WindowStruct -> IO Int32
{-# LINE 690 "Graphics/UI/SDL/Video.hsc" #-}

getWindowDisplayIndex :: Window -> IO Int
getWindowDisplayIndex win =
  withForeignPtr win $ \cw -> do
    ret <- sdlGetWindowDisplayIndex cw
    handleErrorI "getWindowDisplayIndex" ret (return . fromIntegral)

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetWindowID"
  sdlGetWindowID :: Ptr WindowStruct -> IO Word32
{-# LINE 700 "Graphics/UI/SDL/Video.hsc" #-}

getWindowID :: Window -> IO WindowID
getWindowID win =
  withForeignPtr win $ \cw -> fromIntegral <$> sdlGetWindowID cw

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetWindowFromID"
  sdlGetWindowFromID :: Word32 -> IO (Ptr WindowStruct)
{-# LINE 708 "Graphics/UI/SDL/Video.hsc" #-}

getWindowFromID :: WindowID -> IO Window
getWindowFromID wid = do
  cw <- sdlGetWindowFromID (fromIntegral wid)
  handleError "getWindowFromID" cw mkFinalizedWindow

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetDisplayName"
  sdlGetDisplayName :: Int32 -> IO CString
{-# LINE 717 "Graphics/UI/SDL/Video.hsc" #-}

getDisplayName :: Int32 -> IO String
{-# LINE 719 "Graphics/UI/SDL/Video.hsc" #-}
getDisplayName i =
  fatalSDLNull "SDL_GetDisplayName" (sdlGetDisplayName i) >>= peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetNumDisplayModes"
  getNumDisplayModes :: Int32 -> IO Int32
{-# LINE 725 "Graphics/UI/SDL/Video.hsc" #-}
--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetNumVideoDisplays"
  getNumVideoDisplays :: IO Int32
{-# LINE 728 "Graphics/UI/SDL/Video.hsc" #-}

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetCurrentVideoDriver"
  sdlGetCurrentVideoDriver :: IO CString

getCurrentVideoDriver :: IO String
getCurrentVideoDriver = sdlGetCurrentVideoDriver >>= peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetDisplayBounds"
  sdlGetDisplayBounds :: Int32 -> Ptr Rect -> IO Int32
{-# LINE 739 "Graphics/UI/SDL/Video.hsc" #-}

getDisplayBounds :: Int -> IO Rect
getDisplayBounds index =
  alloca $ \rect -> do
    ret <- sdlGetDisplayBounds (fromIntegral index) rect
    handleErrorI "getDisplayBounds" ret $ return $ peek rect

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetNumVideoDrivers"
  getNumVideoDrivers :: IO Int32
{-# LINE 749 "Graphics/UI/SDL/Video.hsc" #-}

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetVideoDriver"
  sdlGetVideoDriver :: Int32 -> IO CString
{-# LINE 753 "Graphics/UI/SDL/Video.hsc" #-}

getVideoDriver :: Int32 -> IO String
{-# LINE 755 "Graphics/UI/SDL/Video.hsc" #-}
getVideoDriver =
  fatalSDLNull "SDL_GetVideoDriver" . sdlGetVideoDriver >=> peekCString

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_GetWindowFlags"
  sdlGetWindowFlags :: Ptr WindowStruct -> IO Word32
{-# LINE 761 "Graphics/UI/SDL/Video.hsc" #-}

getWindowFlags :: Window -> IO [WindowFlag]
getWindowFlags w = withForeignPtr w $
  fmap (fromBitmask windowFlagToC) . sdlGetWindowFlags

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_VideoInit"
  videoInit :: IO ()

--------------------------------------------------------------------------------
foreign import ccall safe "SDL_VideoQuit"
  videoQuit :: IO ()
