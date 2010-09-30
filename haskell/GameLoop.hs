module GameLoop where

import Control.Concurrent.STM
import qualified Graphics.UI.SDL as SDL

import Input

startGame :: (Double -> KeyState -> IO ()) -> IO () -> IO ()
startGame update draw = do
    let playerCount = 2
    playerBindingsVar <- newDefaultPlayerBindings playerCount
    loop (emptyKeyState playerCount) playerBindingsVar
    where
        loop keyState playerBindingsVar = do
            draw
            update 42 keyState
            keyState' <- handleEvents playerBindingsVar keyState
            if systemDown keyState' Menu 
                then loop keyState' playerBindingsVar
                else quit

quit = do
    SDL.quit

