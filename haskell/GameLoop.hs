module GameLoop where

import Control.Concurrent.STM
import qualified Graphics.UI.SDL as SDL
import Data.Time

import Input

startGame :: (Double -> KeyState -> IO ()) -> IO () -> IO ()
startGame update draw = do
    let playerCount = 2
    playerBindingsVar <- newDefaultPlayerBindings playerCount
    now <- getCurrentTime
    loop (emptyKeyState playerCount) playerBindingsVar now
    where
        loop keyState playerBindingsVar before = do
            draw
            now <- getCurrentTime
            let dt = diffTime now before
            update dt keyState
            keyState' <- handleEvents playerBindingsVar keyState
            if systemKeyDown keyState' Menu 
                then loop keyState' playerBindingsVar now
                else quit

quit = do
    SDL.quit

diffTime = (realToFrac .) . diffUTCTime