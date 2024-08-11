import Control.Monad (unless, when)
import GHC.Base (liftM)
import Text.Parsec (State (statePos))
import Prelude

{-

\**************************** IMPORTANT ****************************

This week is a two-step homework. First, you have to solve the
"Maze" challenge, and then the "Forest" challenge. The challenges
are in two separate files in both the homework and solution, so
you can check the solution for the first "Maze" challenge without
spoilers of the "Forest" one. Make sure to check the solution for
"Maze" (and only "Maze," I see you 🥸👀) before starting with the
"Forest" challenge!

\*******************************************************************

Today, you'll build the simplest and most basic game imaginable.
It'll be a maze game where the player has to write a list of moves, and the game will perform them
and tell the player where it ends up. Then, the player can change the moves and check again until it
finds the exit.

To play the game, the player will open GHCi, load this file, and run a "solveMaze" function that
takes a maze and a list of moves and returns a String with the resulting state.

It should look like this:

\*Main> solveMaze testMaze []
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
\*Main> solveMaze testMaze [GoLeft]
"You've hit a wall!"
\*Main> solveMaze testMaze [GoForward]
"You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
\*Main> solveMaze testMaze [GoForward, GoRight]
"You've hit a wall!"
\*Main> solveMaze testMaze [GoForward, GoLeft]
"YOU'VE FOUND THE EXIT!!"

How are you going to achieve this? You can try it on your own, but here you have a
step-by-step just in case:

1. Write two data types. One for the moves (Move) you can make, and another for the maze (Maze).
(Use the example above to figure them out.)

2. Write a function called "move" that takes a maze and a move and returns the maze after the move.

3. Write a "testMaze" value of type "Maze" and test the "move" function in GHCi.

4. Write the "solveMaze" function that will take a maze and a list of moves and returns the maze
after making those moves.

5. If you test the "solveMaze" function, you'll see that each time you try to solve the maze,
it'll print the whole maze for the player to see. To avoid that, write a "showCurrentChoice" function
that takes a maze and returns a different string depending on if you hit a wall, found the exit, or
still need to make another choice.

6. Adapt adapt "solveMaze" function to use "showCurrentChoice" and play with your new game using GHCi! :D
-}

{--
 - NOTE: Maze structure:
 - A `Move` is a sum type of 4 possible values.
 - A cell contains a single valid `Move`
 - A solvable `Maze` is a list of moves composing a continuous path.
 - To solve a maze, the list of user-selected moves must be the same as the Maze.
--}

data Move
  = DLeft
  | DRight
  | DUp
  | DDown
  deriving (Show, Eq)

data Cmd
  = Exit
  | Move Move
  | Check -- to check a list of moves on the maze
  deriving (Show, Eq)

type Maze = [Move]

-- DEBUG
testMaze :: Maze
testMaze = [DUp, DLeft]

testMoves :: [Move]
testMoves = [DUp, DLeft]

isMazeSolvable :: [Move] -> Maze -> Bool
isMazeSolvable moves corrects =
    let z = zip moves corrects 
        fn (m, c) _ = m == c
    in  foldr fn False z

isValidMoveSet moves maze = do
  print $
    "this maze is "
      <> ( if isMazeSolvable moves maze
            then "solvable with those moves."
            else "unsolvable with those moves."
         )

move :: Maze -> Move -> IO Maze
move []          _  = pure []
move maze@(m:ms) m' =
  if m' == m
    then print "Nice! You made the right move!"  >> pure ms
    else print "Oops! Incorrect move!" >> pure maze

{-
 - NOTE: Parsing function. Maps a 1-string to a predefined `Cmd`.
 -}
parseCmd :: String -> Maybe Cmd
parseCmd s = case s of
  "l" -> Just $ Move DLeft
  "r" -> Just $ Move DRight
  "u" -> Just $ Move DUp
  "d" -> Just $ Move DDown
  "q" -> Just Exit
  "c" -> Just Check
  _   -> Nothing

{--
 - MAIN
 --}
app :: Maze -> IO ()
app maze = do
  case maze of
    [] -> putStrLn "You won!"
    _  -> do
      let moves = testMoves
      s <- getLine
      case parseCmd s of
        Just Exit     -> pure ()        -- [x]
        Just Check    -> isValidMoveSet moves maze
                          >>  app maze  -- [x]
        Just (Move d) -> print ("You moved " <> show d)
                          >>  move maze d
                          >>= app       -- [x]
        _             -> print "Invalid move."
                          >>  app maze

main :: IO ()
main = do
  let maze = testMaze
  putStrLn "--------------------------------------------------------------------"
  putStrLn "Welcome to the Maze!"
  putStrLn "- move by entering one letter: u (up), d (down), l (left), r (right)"
  putStrLn "- quit by entering the letter: q (quit)"
  putStrLn "--------------------------------------------------------------------"
  app maze
  putStrLn "Thank you for playing the Maze game! Goodbye!"

{-
 - NOTE: post development
 - - possible improvements
 -  1. parse each command line as a space-delimited strings
 -    - the list can be used to do a solution check on the maze
 -}
