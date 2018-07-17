module SDL
  ( init
  , quit
  , createWindow
  , createRenderer
  , destroyWindow
  , destroyTexture
  , destroyRenderer
  , renderCopy
  , renderClear
  , renderPresent
  , renderFillRect
  , setRenderDrawColor
  , delay
  , pollEvent
  , createTexture
  , loadBMP
  , saveBMP
  , freeSurface

  , windowposUndefinedMask
  , windowposUndefinedDisplay
  , windowposUndefined
  , windowposIsUndefined
  , windowposCenteredMask
  , windowposCenteredDisplay
  , windowposCentered
  , windowposIsCentered

  , InitConfig(..)    , defaultInitConfig     , allInitConfig
  , WindowConfig(..)  , defaultWindowConfig
  , RendererConfig(..), defaultRendererConfig
  , TextureConfig(..) , defaultTextureConfig
  , TextureAccess(..)
  , PixelFormat(..)
  , Event(..)

  , Rect(..)
  , Window(..)
  , Surface(..)
  , Texture(..)
  , Renderer(..)

  , nullPtr
  ) where

import Prelude hiding (init)

import Control.Applicative ((<$>), (<*>))

import Data.Int
import Data.Bits
import Data.Word

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable


data Event
  = None
  | Quit
  | Keydown Int
  | Keyup   Int
  | Unknown
  deriving Show


data Rect = Rect
  { rectX :: Int32
  , rectY :: Int32
  , rectW :: Int32
  , rectH :: Int32
  } deriving Show

instance Storable Rect where
  alignment _ = 4
  sizeOf    _ = 16

  peek ptr = Rect
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr 4
    <*> peekByteOff ptr 8
    <*> peekByteOff ptr 12

  poke ptr (Rect rectX rectY rectW rectH) = do
    pokeByteOff ptr 0  rectX
    pokeByteOff ptr 4  rectY
    pokeByteOff ptr 8  rectW
    pokeByteOff ptr 12 rectH


data RWops
data Window 
data Surface
data Texture
data Renderer



{- https://wiki.libsdl.org/CategoryInit
 -}

foreign import ccall unsafe "SDL_Init" init' :: Word32 -> IO Int32
foreign import ccall unsafe "SDL_Quit"  quit :: IO ()

data InitConfig = InitConfig
  { initTimer          :: Bool
  , initAudio          :: Bool
  , initVideo          :: Bool
  , initJoystick       :: Bool
  , initHaptic         :: Bool
  , initGameController :: Bool
  , initEvents         :: Bool
  } deriving Show

defaultInitConfig =
  InitConfig False False True False False False False

allInitConfig =
  InitConfig True True True True True True True

init :: InitConfig -> IO Int32
init config =
  init' $ timer          .|.
          audio          .|.
          video          .|.
          joystick       .|.
          haptic         .|.
          gameController .|.
          events
  where
    timer          = if initTimer          config then 0x00000001 else 0x00000000
    audio          = if initAudio          config then 0x00000010 else 0x00000000
    video          = if initVideo          config then 0x00000020 else 0x00000000
    joystick       = if initJoystick       config then 0x00000200 else 0x00000000
    haptic         = if initHaptic         config then 0x00001000 else 0x00000000
    gameController = if initGameController config then 0x00002000 else 0x00000000
    events         = if initEvents         config then 0x00004000 else 0x00000000



{- https://wiki.libsdl.org/CategoryTimer
 -}

foreign import ccall unsafe "SDL_Delay"                                     delay :: Word32 -> IO ()
foreign import ccall unsafe "SDL_GetTicks"                               getTicks :: IO Word32
foreign import ccall unsafe "SDL_GetPerformanceCounter"     getPerformanceCounter :: IO Word64
foreign import ccall unsafe "SDL_GetPerformanceFrequency" getPerformanceFrequency :: IO Word64



{- https://wiki.libsdl.org/CategoryError
 -}

foreign import ccall unsafe "SDL_ClearError" clearError :: IO ()
foreign import ccall unsafe "SDL_GetError"    getError' :: IO CString


getError :: IO String
getError =
  getError' >>= peekCString



{- https://wiki.libsdl.org/CategoryEvents
 -}

foreign import ccall unsafe "SDL_PollEvent" pollEvent' :: Ptr a -> IO Int32


pollEvent :: IO Event
pollEvent =
  allocaBytes 56 bytesToArray
  where
    quitEv    = 0x100
    keydownEv = 0x300
    keyupEv   = 0x301

    bytesToArray bytes = do
      hasEvent <- pollEvent' bytes

      if
        hasEvent == 0
      then
        return None
      else
        peek bytes >>= getEvent

    getEvent :: Word32 -> IO Event
    getEvent a
      | a == quitEv    = return $ Quit
      | a == keydownEv = return $ Keydown 0
      | a == keyupEv   = return $ Keyup   0
      | otherwise      = return $ Unknown



{- https://wiki.libsdl.org/CategoryIO
 -}

foreign import ccall unsafe "SDL_RWFromFile" rwFromFile' :: CString -> CString -> IO (Ptr RWops)


rwFromFile :: String -> String -> IO (Ptr RWops)
rwFromFile file mode =
  withCString file $ \fileStr ->
    withCString mode $ \modeStr ->
      rwFromFile' fileStr modeStr



{- https://wiki.libsdl.org/CategoryVideo
 -}

foreign import ccall unsafe "SDL_CreateWindow"              createWindow' :: CString -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> IO (Ptr Window)
foreign import ccall unsafe "SDL_DestroyWindow"             destroyWindow :: Ptr Window -> IO ()
foreign import ccall unsafe "SDL_GetWindowSurface"       getWindowSurface :: Ptr Window -> IO (Ptr Surface)
foreign import ccall unsafe "SDL_UpdateWindowSurface" updateWindowSurface :: Ptr Window -> IO Int32


data WindowConfig = WindowConfig
  { windowTitle  :: String
  , windowX      :: Int32
  , windowY      :: Int32
  , windowWidth  :: Int32
  , windowHeight :: Int32

  -- Flags
  , windowFullscreen        :: Bool
  , windowOpenGL            :: Bool
  , windowShown             :: Bool
  , windowHidden            :: Bool
  , windowBorderless        :: Bool
  , windowResizable         :: Bool
  , windowMinimized         :: Bool
  , windowMaximized         :: Bool
  , windowInputGrabbed      :: Bool
  , windowInputFocus        :: Bool
  , windowMouseFocus        :: Bool
  , windowFullscreenDesktop :: Bool
  , windowForeign           :: Bool
  , windowAllowHighDPI      :: Bool
  } deriving Show

defaultWindowConfig = WindowConfig
  { windowTitle             = "SDL 2 w/ Haskell"
  , windowX                 = windowposUndefined
  , windowY                 = windowposUndefined
  , windowWidth             = 640
  , windowHeight            = 480
  , windowFullscreen        = False
  , windowOpenGL            = False
  , windowShown             = True
  , windowHidden            = False
  , windowBorderless        = False
  , windowResizable         = False
  , windowMinimized         = False
  , windowMaximized         = False
  , windowInputGrabbed      = False
  , windowInputFocus        = False
  , windowMouseFocus        = False
  , windowFullscreenDesktop = False
  , windowForeign           = False
  , windowAllowHighDPI      = False
  }

createWindow :: WindowConfig -> IO (Ptr Window)
createWindow config =
  withCString title $
    \str -> createWindow' str x y w h $ fullscreen        .|.
                                        openGL            .|.
                                        shown             .|.
                                        hidden            .|.
                                        borderless        .|.
                                        resizable         .|.
                                        minimized         .|.
                                        maximized         .|.
                                        inputGrabbed      .|.
                                        inputFocus        .|.
                                        mouseFocus        .|.
                                        fullscreenDesktop .|.
                                        foreign'          .|.
                                        allowHighDPI
  where
    title = windowTitle  config
    x     = windowX      config
    y     = windowY      config
    w     = windowWidth  config
    h     = windowHeight config

    fullscreen        = if windowFullscreen        config then 0x00000001 else 0x00000000
    openGL            = if windowOpenGL            config then 0x00000002 else 0x00000000
    shown             = if windowShown             config then 0x00000004 else 0x00000000
    hidden            = if windowHidden            config then 0x00000008 else 0x00000000
    borderless        = if windowBorderless        config then 0x00000010 else 0x00000000
    resizable         = if windowResizable         config then 0x00000020 else 0x00000000
    minimized         = if windowMinimized         config then 0x00000040 else 0x00000000
    maximized         = if windowMaximized         config then 0x00000080 else 0x00000000
    inputGrabbed      = if windowInputGrabbed      config then 0x00000100 else 0x00000000
    inputFocus        = if windowInputFocus        config then 0x00000200 else 0x00000000
    mouseFocus        = if windowMouseFocus        config then 0x00000400 else 0x00000000
    fullscreenDesktop = if windowFullscreenDesktop config then 0x00001001 else 0x00000000
    foreign'          = if windowForeign           config then 0x00000800 else 0x00000000
    allowHighDPI      = if windowAllowHighDPI      config then 0x00002000 else 0x00000000


windowposUndefinedMask :: Int32
windowposUndefinedMask =
  0x1FFF0000


windowposUndefinedDisplay :: Int32 -> Int32
windowposUndefinedDisplay x =
  windowposUndefinedMask .|. x


windowposUndefined :: Int32
windowposUndefined =
  windowposUndefinedDisplay 0


windowposIsUndefined :: Int32 -> Bool
windowposIsUndefined x =
  ((fromIntegral x :: Word32) .&. 0xFFFF0000) == fromIntegral windowposUndefinedMask


windowposCenteredMask :: Int32
windowposCenteredMask =
  0x2FFF0000


windowposCenteredDisplay :: Int32 -> Int32
windowposCenteredDisplay x =
  windowposCenteredMask .|. x


windowposCentered :: Int32
windowposCentered =
  windowposCenteredDisplay 0


windowposIsCentered :: Int32 -> Bool
windowposIsCentered x =
  ((fromIntegral x :: Word32) .&. 0xFFFF0000) == fromIntegral windowposCenteredMask



{- https://wiki.libsdl.org/CategoryPixels
 -}

data PixelFormat
  = PixelFormatUnknown
  | PixelFormatIndex1LSB
  | PixelFormatIndex1MSB
  | PixelFormatIndex4LSB
  | PixelFormatIndex4MSB
  | PixelFormatIndex8
  | PixelFormatRGB332
  | PixelFormatRGB444
  | PixelFormatRGB555
  | PixelFormatBGR555
  | PixelFormatARGB4444
  | PixelFormatRGBA4444 
  | PixelFormatABGR4444
  | PixelFormatBGRA4444
  | PixelFormatARGB1555
  | PixelFormatRGBA5551
  | PixelFormatABGR1555
  | PixelFormatBGRA5551
  | PixelFormatRGB565
  | PixelFormatBGR565
  | PixelFormatRGB24
  | PixelFormatBGR24
  | PixelFormatRGB888
  | PixelFormatRGBX8888
  | PixelFormatBGR888
  | PixelFormatBGRX8888
  | PixelFormatARGB8888
  | PixelFormatRGBA8888
  | PixelFormatABGR8888
  | PixelFormatBGRA8888
  | PixelFormatARGB2101010
  | PixelFormatRGBA32
  | PixelFormatARGB32
  | PixelFormatBGRA32
  | PixelFormatABGR32
  | PixelFormatYV12
  | PixelFormatIYUV
  | PixelFormatYUY2
  | PixelFormatUYVY
  | PixelFormatYVYU
  | PixelFormatNV12
  | PixelFormatNV21
  | PixelFormatExternalOES
  deriving Show



{- https://wiki.libsdl.org/CategorySurface
 -}

foreign import ccall unsafe "SDL_FreeSurface" freeSurface :: Ptr Surface -> IO ()
foreign import ccall unsafe "SDL_LoadBMP_RW"   loadBMP_RW :: Ptr RWops -> Int32 -> IO (Ptr Surface)
foreign import ccall unsafe "SDL_SaveBMP_RW"   saveBMP_RW :: Ptr Surface -> Ptr RWops -> Int32 -> IO Int32


loadBMP :: String -> IO (Ptr Surface)
loadBMP file = do
  file' <- rwFromFile file "rb"
  loadBMP_RW file' 1


saveBMP :: Ptr Surface -> String -> IO Int32
saveBMP surface file = do
  file' <- rwFromFile file "wb"
  saveBMP_RW surface file' 1



{- https://wiki.libsdl.org/CategoryRender
 -}

foreign import ccall unsafe "SDL_CreateRenderer"          createRenderer'  :: Ptr Window -> Int32 -> Word32 -> IO (Ptr Renderer)
foreign import ccall unsafe "SDL_DestroyRenderer"         destroyRenderer  :: Ptr Renderer -> IO ()
foreign import ccall unsafe "SDL_DestroyTexture"           destroyTexture  :: Ptr Texture -> IO ()
foreign import ccall unsafe "SDL_RenderCopy"                  renderCopy'  :: Ptr Renderer -> Ptr Texture -> Ptr Rect -> Ptr Rect -> IO Int32
foreign import ccall unsafe "SDL_RenderClear"                 renderClear  :: Ptr Renderer -> IO Int32
foreign import ccall unsafe "SDL_RenderPresent"             renderPresent  :: Ptr Renderer -> IO ()
foreign import ccall unsafe "SDL_RenderFillRect"          renderFillRect'  :: Ptr Renderer -> Ptr Rect -> IO Int32
foreign import ccall unsafe "SDL_SetRenderDrawColor"   setRenderDrawColor  :: Ptr Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO Int32
foreign import ccall unsafe "SDL_UpdateTextrue"            updateTexture'  :: Ptr Texture -> Ptr Rect -> Ptr Word8 -> Int32 -> IO Int32
foreign import ccall unsafe "SDL_CreateTexture"            createTexture'  :: Ptr Renderer -> Word32 -> Int32 -> Int32 -> Int32 -> IO (Ptr Texture)
foreign import ccall unsafe "SDL_CreateTextureFromSurface" createTexture'' :: Ptr Renderer -> Ptr Surface -> IO (Ptr Texture)


data TextureAccess
  = TextureAccessStatic
  | TextureAccessStreaming
  | TextureAccessTarget
  deriving Show

data TextureConfig = TextureConfig
  { texSurface  :: Ptr Surface
  , texRenderer :: Ptr Renderer
  , texAccess   :: TextureAccess
  , texFormat   :: PixelFormat
  , texWidth    :: Int32
  , texHeight   :: Int32
  } deriving Show

defaultTextureConfig = TextureConfig
  { texSurface  = nullPtr
  , texRenderer = nullPtr
  , texAccess   = TextureAccessStatic
  , texFormat   = PixelFormatUnknown
  , texWidth    = 0
  , texHeight   = 0
  }

createTexture :: TextureConfig -> IO (Ptr Texture)
createTexture config =
  if surface == nullPtr
  then createTexture' renderer format access width height
  else createTexture'' renderer surface
  where
    renderer = (texRenderer config)
    surface  = (texSurface config)
    width    = (texWidth config)
    height   = (texHeight config)
    access   = getAccess (texAccess config)
    format   = getFormat (texFormat config)

    getAccess TextureAccessStatic    = 0
    getAccess TextureAccessStreaming = 1
    getAccess TextureAccessTarget    = 2

    getFormat PixelFormatUnknown     = 0x00000000
    getFormat PixelFormatIndex1LSB   = 0x11100100
    getFormat PixelFormatIndex1MSB   = 0x11200100
    getFormat PixelFormatIndex4LSB   = 0x12100400
    getFormat PixelFormatIndex4MSB   = 0x12200400
    getFormat PixelFormatIndex8      = 0x13000801
    getFormat PixelFormatRGB332      = 0x14110801
    getFormat PixelFormatRGB444      = 0x15120C02
    getFormat PixelFormatRGB555      = 0x15130F02
    getFormat PixelFormatBGR555      = 0x15530F02
    getFormat PixelFormatARGB4444    = 0x15321002
    getFormat PixelFormatRGBA4444    = 0x15421002
    getFormat PixelFormatABGR4444    = 0x15721002
    getFormat PixelFormatBGRA4444    = 0x15821002
    getFormat PixelFormatARGB1555    = 0x15331002
    getFormat PixelFormatRGBA5551    = 0x15441002
    getFormat PixelFormatABGR1555    = 0x15731002
    getFormat PixelFormatBGRA5551    = 0x15841002
    getFormat PixelFormatRGB565      = 0x15151002
    getFormat PixelFormatBGR565      = 0x15551002
    getFormat PixelFormatRGB24       = 0x17101803
    getFormat PixelFormatBGR24       = 0x17401803
    getFormat PixelFormatRGB888      = 0x16161804
    getFormat PixelFormatRGBX8888    = 0x16261804
    getFormat PixelFormatBGR888      = 0x16561804
    getFormat PixelFormatBGRX8888    = 0x16661804
    getFormat PixelFormatARGB8888    = 0x16362004
    getFormat PixelFormatRGBA8888    = 0x16462004
    getFormat PixelFormatABGR8888    = 0x16762004
    getFormat PixelFormatBGRA8888    = 0x16862004
    getFormat PixelFormatARGB2101010 = 0x16372004
    getFormat PixelFormatRGBA32      = 0x16762004
    getFormat PixelFormatARGB32      = 0x16862004
    getFormat PixelFormatBGRA32      = 0x16362004
    getFormat PixelFormatABGR32      = 0x16462004
    getFormat PixelFormatYV12        = 0x32315659
    getFormat PixelFormatIYUV        = 0x56555949
    getFormat PixelFormatYUY2        = 0x32595559
    getFormat PixelFormatUYVY        = 0x59565955
    getFormat PixelFormatYVYU        = 0x55595659
    getFormat PixelFormatNV12        = 0x3231564E
    getFormat PixelFormatNV21        = 0x3132564E
    getFormat PixelFormatExternalOES = 0x2053454F


data RendererConfig = RendererConfig
  { rendererWindow   :: Ptr Window
  , rendererGPUDev   :: Int32

  -- Flags
  , rendererSoftware      :: Bool
  , rendererAccelerated   :: Bool
  , rendererPresentVSync  :: Bool
  , rendererTargetTexture :: Bool
  } deriving Show

defaultRendererConfig = RendererConfig
  { rendererWindow        = nullPtr
  , rendererGPUDev        = -1
  , rendererSoftware      = False
  , rendererAccelerated   = True
  , rendererPresentVSync  = True
  , rendererTargetTexture = False
  }

createRenderer :: RendererConfig -> IO (Ptr Renderer)
createRenderer config =
  createRenderer' win dev $ software      .|.
                            accelerated   .|.
                            presentVSync  .|.
                            targetTexture
  where
    win = rendererWindow config
    dev = rendererGPUDev config

    software      = if rendererSoftware      config then 0x00000001 else 0x00000000
    accelerated   = if rendererAccelerated   config then 0x00000002 else 0x00000000
    presentVSync  = if rendererPresentVSync  config then 0x00000004 else 0x00000000
    targetTexture = if rendererTargetTexture config then 0x00000008 else 0x00000000


renderFillRect :: Ptr Renderer -> Rect -> IO Int32
renderFillRect ren rect =
  with rect $ \ptrRect ->
    renderFillRect' ren ptrRect


renderCopy :: Ptr Renderer -> Ptr Texture -> Rect -> Rect -> IO Int32
renderCopy ren tex src dst =
  with src $ \ptrSrc ->
    with dst $ \ptrDst ->
      renderCopy' ren tex ptrSrc ptrDst
