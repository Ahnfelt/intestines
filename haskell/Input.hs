import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

data Input = Input {
    players :: [PlayerKeys],
    system :: SystemKeys
    }

data PlayerKey 
    = Up 
    | Down 
    | Left 
    | Right 
    | Primary 
    | Secondary
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data SystemKey 
    = Menu
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

type PlayerConfiguration = PlayerKey -> SDL.SDLKey
type PlayerKeys = Set PlayerKey
type SystemKeys = Set SystemKey

systemConfiguration key = case key of
    Menu -> SDLK_ESCAPE

defaultConfigurations = [
    \key -> case key of
        Up -> SDLK_UP
        Down -> SDLK_DOWN
        Left -> SDLK_LEFT
        Right -> SDLK_RIGHT
        Primary -> SDLK_RSHIFT
        Secondary -> SDLK_RETURN,
    \key -> case key of
        Up -> SDLK_e
        Down -> SDLK_d
        Left -> SDLK_s
        Right -> SDLK_f
        Primary -> SDLK_a
        Secondary -> SDLK_q]

inverseConfiguration :: (a -> b) -> Map b a
inverseConfiguration configuration = Map.fromList $ map (\k -> (configuration k, k)) [minBound..maxBound]

handleEvents configurations input = do
    event <- SDL.pollEvent
    if event /= SDL.NoEvent
        then do
            input' <- handleEvent configurations input event
            handleEvents configurations input'
        else return input

handleEvent configurations input event = do
    case event of
        SDL.KeyDown SDL.Keysym { SDL.symKey = key } -> return $ press key True
        SDL.KeyUp SDL.Keysym { SDL.symKey = key } -> return $ press key False
        SDL.Quit -> return input
        _ -> return input
    where
        press key pressed = systemPress' key pressed $ playerPress' key pressed input
        systemPress' key pressed input = input { system = systemPress key pressed (system input) }
        playerPress' key pressed input = input { players = map (playerPress key pressed) (zip configurations (players input)) }


playerPress :: SDL.SDLKey -> Bool -> (PlayerConfiguration, PlayerKeys) -> PlayerKeys
playerPress key pressed (configuration, playerKeys) =
    let keys = inverseConfiguration configuration in
    case Map.lookup key keys of
        Just key -> pressFunction pressed key playerKeys 
        Nothing -> playerKeys

systemPress :: SDL.SDLKey -> Bool -> SystemKeys -> SystemKeys
systemPress key pressed systemKeys = 
    let keys = inverseConfiguration systemConfiguration in
    case Map.lookup key keys of
        Just key -> pressFunction pressed key systemKeys 
        Nothing -> systemKeys

pressFunction :: Bool -> a -> Set a -> Set a
pressFunction True = Set.insert
pressFunction False = Set.delete

