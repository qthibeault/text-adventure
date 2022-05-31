module Main where

import Data.Char   (toLower)
import System.IO   (hFlush, stdout)
import Text.Printf (printf)

data GameAction = Attack | Run

data GameState = GameState {
    playerHealth :: Int,
    enemyHealth  :: Int
}

lowerString :: String -> String
lowerString = map toLower

parseAction :: String -> Maybe GameAction
parseAction "attack" = Just Attack
parseAction "run"    = Just Run
parseAction _        = Nothing

readAction :: IO (Maybe GameAction)
readAction = parseAction . lowerString <$> getLine

playerPrompt :: GameState -> IO (Maybe GameAction)
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
        case action of Just Attack -> step $ updateState state (-1) (-2)
                       Just Run    -> putStrLn "Fled"
                       Nothing     -> putStrLn "Unknown command"

main :: IO ()
main = step initialState
    where initialState = GameState { playerHealth = 10, enemyHealth = 8 }
