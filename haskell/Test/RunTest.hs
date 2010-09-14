

import Feature
import qualified Entity.Player as Player
import qualified Feature.Position as Position
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

run n = do
    p <- atomically Player.new
    replicateM_ n $ do 
        atomically $ updateEntity p
        let Just pos = getFeature p
        pos' <- atomically $ get Position.position pos
        print pos'

