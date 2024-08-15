module Main where

import System.Directory (doesFileExist)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORTANT: Read the README.md path before completing the homework.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- This is a CLI application that allows the user to manage a TODO list.
-- The user can add and remove items from the list and save and load the list
-- from a path.
-- It's a working prototype, but it has some bugs. Specifically, it doesn't
-- handle errors very well. Your mission, should you choose to accept it, is to
-- fix those bugs and make the application more robust. Hint: Try to interact
-- with the application in unexpected ways and see what happens! You should be
-- able to find and fix 3 bugs.

printTodoItem :: (Int, String) -> IO ()
printTodoItem (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
  putStrLn ""
  putStrLn "Current TODO list:"
  foldr (\x k -> printTodoItem x >> k) (return ()) (zip [0 ..] todos)
  command <- getLine
  interpretCommand command todos

delete :: Int -> [a] -> [a]
delete 0 (_ : as) = as
delete _ [] = []
delete n (a : as) = a : delete (n - 1) as

interpretCommand :: String -> [String] -> IO ()
interpretCommand cmd todos = case cmd of
  "q" -> return ()
  ('+' : ' ' : todo) -> prompt (todo : todos)
  ('-' : ' ' : num) -> case (readMaybe num :: Maybe Int) of
    Nothing -> prompt todos
    _ -> prompt $ delete (read num) todos -- FIX: 1, 2
  ('s' : ' ' : path) -> do
    -- FIX: 5
    isPath <- doesFileExist path
    if isPath
      then do
        print "Are you sure you want to overwrite the current file?"
        confirm <- getLine
        case confirm of
          "yes" -> overwrite
          _ -> prompt todos
      else
        overwrite
   where
    overwrite = writeFile path (show todos) >> prompt todos -- FIX: 3
  ('l' : ' ' : path) -> do
    isPath <- doesFileExist path
    if isPath
      then readFile path >>= prompt . read -- FIX: 4
      else putStrLn "File does not exist. Please enter a valid file path." >> prompt todos
  _ -> putStrLn ("Invalid command: `" ++ cmd ++ "`") >> prompt todos

printCommands :: IO ()
printCommands = do
  putStrLn "Commands:"
  putStrLn "+ <Item Name>   - Add a TODO entry"
  putStrLn "- <Item Number> - Delete the numbered entry"
  putStrLn "s <File Name>   - Save the current list of TODOs"
  putStrLn "l <File Name>   - Load the saved list of TODOs"
  putStrLn "q               - Quit without saving"

{-
 - TODO
 -  1. [x] app crashes on when using delete option after loading file
 -    - problem description:
 -      1. load the storage file into app.
 -      2. use the delete option with _any_ non-Int string.
 -      3. app crashes.
 -    - expeced behavior:
 -      - after loading the list, the app should handle when the item does not match.
 -  2. (same as 1) [x] no handling for non Int input when using delete option
 -  3. [x] app quits on save
 -    - expected behavior:
 -      - it should loop back after saving the new state to specified filepath
 -  4. [x] app quits when load a file that does not exist.
 -    - problem: no handling when file does not exist
 -    - expected: handle it.
 -  5. [x] do a file check before saving to ensure valid overwriting commit.
 -  6. [ ] refactor `interpretCommand` to be leaner.
 -}

main :: IO ()
main = do
  printCommands
  prompt []
