module Main where

import System.IO
import Text.Printf (printf)
import Data.Char (toLower)

data GameAction = Attack | Run

data GameState = GameState {
    playerHealth :: Int,
    enemyHealth :: Int
}

lowerString :: String -> String 
lowerString = map toLower

parseAction :: String -> GameAction
parseAction "attack" = Attack
parseAction "run" = Run
parseAction _ = error "Invalid action"

readAction :: IO GameAction
readAction = parseAction . lowerString <$> getLine

playerPrompt :: GameState -> IO GameAction 
playerPrompt state = do
    putStr prompt
    hFlush stdout
    readAction
    where prompt = printf "Player: %d/Enemy: %d>" (playerHealth state) (enemyHealth state)

updateState :: GameState -> Int -> Int -> GameState
updateState state playerDelta enemyDelta = GameState {
    playerHealth = playerHealth state + playerDelta,
    enemyHealth = enemyHealth state + enemyDelta
}

step :: GameState -> IO ()
step state
    | playerHealth state <= 0 = putStrLn "YOU DIED"
    | enemyHealth state <= 0 = putStrLn "GREAT ENEMY DEFEATED"
    | otherwise = do
        action <- playerPrompt state
        case action of Attack -> step $ updateState state (-1) (-2)
                       Run -> putStrLn "Fled"

main :: IO ()
main = step initialState
    where initialState = GameState { playerHealth = 10, enemyHealth = 8 }
