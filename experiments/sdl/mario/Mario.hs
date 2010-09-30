
import Prelude
import Graphics.UI.SDL as SDL
import Control.Monad

backgroundImage = "background.bmp"
playerImage = "mario.bmp"

main = do
    SDL.init [InitEverything]
    setVideoMode 1000 1000 32 []
    player <- loadBMP playerImage
    background <- loadBMP backgroundImage
    screen <- getVideoSurface
    mapM_ (drawPlayer player background screen) [(x, y) | x <- [0..0], y <- [0..200]]
    quitHandler
    where
        drawPlayer player background screen (x, y) = do
            blitSurface background Nothing screen Nothing
            mapM_ (\d -> (do
               blitSurface player Nothing screen (Just (Rect (x+d) (y+d) 0 0)))) [1..1000]
            --delay 10
            SDL.flip screen



quitHandler :: IO ()
quitHandler = do
    e <- waitEvent
    case e of
        Quit -> return ()
        otherwise -> quitHandler
