module Main where

import SDL


data Input = Input
  { inA     :: Bool
  , inB     :: Bool
  , inUp    :: Bool
  , inDown  :: Bool
  , inLeft  :: Bool
  , inRight :: Bool
  , inClose :: Bool
  } deriving Show

defaultInput =
  Input False False
        False False
        False False
        False


main = do
  res <- SDL.init           SDL.defaultInitConfig
  win <- SDL.createWindow   SDL.defaultWindowConfig
  ren <- SDL.createRenderer SDL.defaultRendererConfig { SDL.rendererWindow = win }

  -- Load surfaces from bitmaps
  sA1 <- SDL.loadBMP "A1.bmp"
  sA2 <- SDL.loadBMP "A2.bmp"
  sA4 <- SDL.loadBMP "A4.bmp"
  sA5 <- SDL.loadBMP "A5.bmp"
  sB  <- SDL.loadBMP "B.bmp"

  -- Create the textures
  a1 <- SDL.createTexture SDL.defaultTextureConfig { SDL.texRenderer = ren
                                                   , SDL.texSurface  = sA1 }
  a2 <- SDL.createTexture SDL.defaultTextureConfig { SDL.texRenderer = ren
                                                   , SDL.texSurface  = sA2 }
  a4 <- SDL.createTexture SDL.defaultTextureConfig { SDL.texRenderer = ren  
                                                   , SDL.texSurface  = sA4 }
  a5 <- SDL.createTexture SDL.defaultTextureConfig { SDL.texRenderer = ren
                                                   , SDL.texSurface  = sA5 }
  b  <- SDL.createTexture SDL.defaultTextureConfig { SDL.texRenderer = ren  
                                                   , SDL.texSurface  = sB }

  -- Release the surfaces now that they're textures.
  SDL.freeSurface sA1
  SDL.freeSurface sA2
  SDL.freeSurface sA4
  SDL.freeSurface sA5
  SDL.freeSurface sB

  -- Tiles & Sprites
  let
    makeSprite tex src w h =
      \x y ->
        SDL.renderCopy ren tex src (SDL.Rect x y w h)

    t1 = makeSprite a1 (SDL.Rect  0  0 16 16) 32 32
    t2 = makeSprite a1 (SDL.Rect 16  0 16 16) 32 32
    t3 = makeSprite a1 (SDL.Rect  0 16 16 16) 32 32
    t4 = makeSprite a1 (SDL.Rect 16 16 16 16) 32 32

  let mainLoop i = do
        e  <- SDL.pollEvent
        i' <- handleEvent i e

        if inClose i'
        then do
          SDL.destroyTexture a1
          SDL.destroyTexture a2
          SDL.destroyTexture a4
          SDL.destroyTexture a5
          SDL.destroyTexture b

          SDL.destroyRenderer ren
          SDL.destroyWindow win
          SDL.quit
        else do
          SDL.renderClear ren

          if inA i'
          then
            return 0
          else do
            t1  0  0
            t2 32  0
            t3  0 32
            t4 32 32

          SDL.renderPresent ren
          mainLoop i'

  mainLoop defaultInput


{-
 Event Processing
 -}

-- These are base cases for the recursion
handleEvent input SDL.None = return input
handleEvent input SDL.Quit = return input { inClose = True }

-- 
handleEvent input (SDL.Keyup k) =
  SDL.pollEvent >>= handleEvent input { inA = False }

-- 
handleEvent input (SDL.Keydown k) =
  SDL.pollEvent >>= handleEvent input { inA = True }

-- This skips events not handled above.
handleEvent input _ =
  SDL.pollEvent >>= handleEvent input
