module Input where

import Control.Concurrent.STM
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

data KeyState = KeyState {
    players :: [PlayerKeys],
    system :: SystemKeys
    }

emptyKeyState n = KeyState {players = take n $ repeat Set.empty, system = Set.empty} 


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

type PlayerKeyBindings = PlayerKey -> SDL.SDLKey
type PlayerKeys = Set PlayerKey
type SystemKeys = Set SystemKey

systemBindings :: SystemKey -> SDLKey
systemBindings key = case key of
    Menu -> SDLK_ESCAPE

newDefaultPlayerBindings :: Int -> IO (TVar [PlayerKeyBindings])
newDefaultPlayerBindings n = atomically $ newTVar $ defaultPlayerBindings n

defaultPlayerBindings n = take n [
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

inverseBindings :: (Enum a, Bounded a, Ord b) => (a -> b) -> Map b a
inverseBindings bindings = Map.fromList $ map (\k -> (bindings k, k)) [minBound..maxBound]

pollEvents :: IO [SDL.Event]
pollEvents = do
    events <- sequence $ repeat SDL.pollEvent
    return $ takeWhile (/= SDL.NoEvent) events

handleEvents :: (TVar [PlayerKeyBindings]) -> KeyState -> IO KeyState
handleEvents bindingsVar keyState = do 
    events <- pollEvents
    bindings <- atomically $ readTVar bindingsVar
    return $ foldl (\ks e -> handleEvent bindings ks e) keyState events

handleEvent :: [PlayerKeyBindings] -> KeyState -> SDL.Event ->  KeyState
handleEvent bindings keyState event = 
    case event of
        SDL.KeyDown SDL.Keysym { SDL.symKey = key } -> press key True
        SDL.KeyUp SDL.Keysym { SDL.symKey = key } -> press key False
        SDL.Quit -> keyState
        _ -> keyState
    where
        press key pressed = systemPress' key pressed $ playerPress' key pressed keyState
        systemPress' key pressed keyState = keyState { system = systemPress key pressed (system keyState) }
        playerPress' key pressed keyState = keyState { players = map (playerPress key pressed) (zip bindings (players keyState)) }


playerPress :: SDL.SDLKey -> Bool -> (PlayerKeyBindings, PlayerKeys) -> PlayerKeys
playerPress key pressed (bindings, playerKeys) =
    let keys = inverseBindings bindings in
    case Map.lookup key keys of
        Just key -> pressFunction pressed key playerKeys 
        Nothing -> playerKeys

systemPress :: SDL.SDLKey -> Bool -> SystemKeys -> SystemKeys
systemPress key pressed systemKeys = 
    let keys = inverseBindings systemBindings in
    case Map.lookup key keys of
        Just key -> pressFunction pressed key systemKeys 
        Nothing -> systemKeys

pressFunction :: Ord a => Bool -> a -> Set a -> Set a
pressFunction True = Set.insert
pressFunction False = Set.delete

systemDown :: KeyState -> SystemKey -> Bool
systemDown keyState systemKey = Set.member systemKey (system keyState)