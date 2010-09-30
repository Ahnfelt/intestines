
startGame :: (Double -> Map Int Keys -> IO ()) -> IO () -> IO ()
startGame update draw = loop
    where
        loop = do
            draw
            update
            (continue, keys) <- handleEvents keys
            if continue then loop else quit

quit = do
    SDL.quit

