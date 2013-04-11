module Main where


--------------------------------------------------------------------------------
-- Filename:            game1.hs
-- Author:              Ron Watkins (latermuse)
-- Date Created:        Thu Nov 15 00:58:34 CST 2012
-- Last modified:       Thu Apr 11 10:10:56 CST 2013
-- Description:
--      This is the most simple game logic I could think of. To play, just load
--      the program and it will ask you to make a move and give you the choice
--      of 'h' or 'l'. 
--      'h' moves left
--      'l' moves right. 
--
--      The game map is just a linked list at the moment.
--      Your position is represented by '1', and empty spaces are represented by
--      '0'. 
--------------------------------------------------------------------------------

import System.IO

gameMap :: Num a => [a]
gameMap = [0,0,0,0,0]

clearScreen :: IO ()
clearScreen = putStr "\ESC[H\ESC[J"

startingPosition :: Int
startingPosition = 1

putPlayer :: (Num a, Show a) => Int -> a -> IO ()
putPlayer n character = do
    let mapSplit = splitAt n gameMap
    let newMap = concat $ fst mapSplit : [character] : snd mapSplit : []
    putStrLn $ show newMap

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    clearScreen
    putStrLn "Welcome to the list"
    gameLoop startingPosition 

gameLoop :: Int -> IO a
gameLoop n = do
    putStrLn "Make a move (h, l)"
    movement <- getChar 
    clearScreen
    playerPosition <- gameLogic movement n 
    gameLoop playerPosition
  where
    gameLogic movement n 
      | movement == 'l' = do
            putPlayer (n + 1) 1
            return (n + 1)
      | movement == 'h' = do
            putPlayer (n - 1) 1
            return (n - 1)
      | otherwise = return n
