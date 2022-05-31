module Main where

import Data.Char     (toLower)
import System.IO
import System.Random
import Text.Printf   (printf)

data GameAction = Attack | Run

data GameState = GameState {
    playerHealth :: Int,
    playerAttack :: (Int, Int),
    enemyHealth  :: Int,
    enemyAttack  :: (Int, Int),
    rng          :: StdGen
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

doAttack :: GameState -> IO GameState
doAttack state = do
    putStrLn (printf "Player does %d damage" enemyDelta)
    putStrLn (printf "Enemy does %d damage" playerDelta)

    return state {
        playerHealth = playerHealth state + playerDelta,
        enemyHealth = enemyHealth state + enemyDelta,
        rng = finalRng
    }
    where (playerDelta, newRng) = uniformR (enemyAttack state) (rng state)
          (enemyDelta, finalRng) = uniformR (playerAttack state) newRng

step :: GameState -> IO ()
step state
    | playerHealth state <= 0 = putStrLn "YOU DIED"
    | enemyHealth state <= 0 = putStrLn "GREAT ENEMY DEFEATED"
    | otherwise = do
        action <- playerPrompt state
        case action of Just Attack -> doAttack state >>= step
                       Just Run    -> putStrLn "Fled"
                       Nothing     -> putStrLn "Unknown command" >>= \x -> step state

main :: IO ()
main = step initialState
    where initialState = GameState {
        playerHealth = 10,
        playerAttack = (1, 4),
        enemyHealth = 8,
        enemyAttack = (1, 2),
        rng = mkStdGen 100
    }
