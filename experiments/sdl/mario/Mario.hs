import Graphics.Rendering.OpenGL
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.DrawingCombinators.Affine as Affine
import Graphics.DrawingCombinators ((%%))
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Set as Set
import Data.Set

backgroundImage = "background.bmp"
playerImage = "mario.png"

windowWidth = 1000
windowHeight = 1000

main = do
    initializeSDL
    initializeOpenGL
    player <- Draw.openSprite playerImage
    background <- Draw.openSprite backgroundImage
    keyVar <- atomically $ newTVar Set.empty
    --forkIO (eventHandler keyVar)
    drawPlayer keyVar player background (0, 0)
    quit
    where
        drawPlayer keyVar player background (x, y) = do
            (x', y') <- atomically $ do
                keys <- readTVar keyVar
                return (
                    if SDLK_LEFT `Set.member` keys then -0.01 else if SDLK_RIGHT `Set.member` keys then 0.01 else 0, 
                    if SDLK_DOWN `Set.member` keys then -0.01 else if SDLK_UP `Set.member` keys then 0.01 else 0)
            Draw.clearRender $ Draw.mconcat [
                (Affine.translate (x, y) `Draw.compose` Affine.scale 0.1 0.1) %% Draw.sprite player,
                Draw.sprite background]
            SDL.glSwapBuffers
            event <- SDL.pollEvent
            continue <- if event /= SDL.NoEvent then handleEvent keyVar event else return True
            when continue $ drawPlayer keyVar player background (x + x', y + y')

eventHandler keyVar = do
    event <- SDL.waitEvent
    continue <- handleEvent keyVar event
    if continue then eventHandler keyVar else quit

initializeSDL = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode windowWidth windowHeight 32 [SDL.OpenGL]

initializeOpenGL = do
    blend $= Enabled
    blendEquation $= FuncAdd
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    textureFunction $= Replace
    texture Texture2D $= Enabled

