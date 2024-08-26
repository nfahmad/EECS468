{-
    Name of program contained in the file: Assignment9.hs
    Brief description of the program: The aim of this project is to create a Haskell program that implements the game of nim.
                                      Players take turns removing stars from rows. 
                                      The game continues until all rows are empty, and the player who takes the last star(s) wins
    Inputs: None
    Output: Information to the player regarding the status of the game
    All collaborators: N/A
    Other Sources for the Code: Stack Overflow, W3Schools, ZVON.org, ChatGPT, "Haskell Interactive Programming" Canvas Slides, "Haskell Monads" Canvas Slides
    Author's full name: Nabeel Ahmad
    Creation date: 04/29/2024
-}

--Import isDigit from the module Data.Char. The function isDigit will be used to determine whether a given character is a digit (0-9) or not
import Data.Char (isDigit)
--Defines a type "Board" for a list of integers
type Board = [Int]
--This will create an initial board
initial :: Board
--This is the initial configuration where each row contains a decreasing number of stars
initial = [5,4,3,2,1]
--This is the function to start the game
nim :: IO ()
--Begin the game of nim
nim = do
    --Put a blank line before the board displays
    putStrLn ""
    --Invoke the game nim itself by invoking the game loop with the initial board and player number
    play initial 1
--This is a recursive main game loop which takes the current board and player number as arguments
play :: Board -> Int -> IO ()
--Play the game of nim
play board player = do
    --This will display the current board
    displayBoard board
    --Put a blank line for visual clarity 
    putStrLn ""
    --Ask the player for their row number selection
    putStr $ "Player " ++ show player ++ "\nEnter a row number: "
    --Get a valid row number from the player
    row <- getValidMove determineValidRow
    --Ask the player how many stars they would like to remove
    putStr $ "Stars to remove: "
    --Get a valid star amount from the player based on the row selection
    stars <- getValidMove (determineValidStars board row)
    --Put a blank line for visual clarity 
    putStrLn ""
    --Update the board after the player makes their move
    let updatedBoard = removeStars board row stars
    --This will check if the game is finished using "gameFinished"
    if gameFinished updatedBoard
        --If the game is finished, then show who won
        then putStrLn $ "Player " ++ show player ++ " wins!"
        --Otherwise, continue playing with the next player
        else play updatedBoard (if player == 1 then 2 else 1)
--This will get a valid move from the player
getValidMove :: (String -> Bool) -> IO Int
--Define the getValidMove function with a parameter determineValidMove, a function to validate user input
getValidMove determineValidMove = do
    --Read the input from the player
    input <- getLine
    --Determine if the move is valid
    if determineValidMove input
        --If the move is valid, return it
        then return (read input)
        --Otherwise, run this
        else do
            --Where the program will indicate that this is an invalid move
            putStrLn "ERROR: Invalid move"
            --And then request a different move
            getValidMove determineValidMove
--This is a function to determine if the entered row number is valid
determineValidRow :: String -> Bool
--Ensures that all characters in the input are digits and that the parsed integer value is within the inclusive range from 1 to 5
determineValidRow input = all isDigit input && read input >= 1 && read input <= 5
--This is a function to determine if the entered number of stars is valid for the given row
determineValidStars :: Board -> Int -> String -> Bool
{-
    Checks if the entered number of stars is valid for a given row.
    And ensures that all characters in the input are digits and that the parsed integer value is within the inclusive range from 1 to the number of stars
-}
determineValidStars board row input = all isDigit input && read input >= 1 && read input <= board !! (row - 1)
--This is a function to display the current board
displayBoard :: Board -> IO ()
{-
    Takes the current state of the game board to display the correct board. This will take a list of IO actions and execute them sequentially.
    It will then iterate over each element of the list, where "i" represents the row number and "n" represents the number of stars in that row.
    Zip is then used to pair each row number with the number of stars in that row. Then, it will generate a string representation of each row
-}
displayBoard board = sequence_ [putStrLn $ show i ++ ": " ++ starRow n | (i, n) <- zip [1..] board]
    --Using starRow
    where
        --Uses "replicate" to help display the stars for each row 
        starRow n = concat (replicate n "* ") 
--This is a function to remove stars from a specific row
removeStars :: Board -> Int -> Int -> Board
--Declaration of the removeStars function
removeStars board row stars =
    --Take elements before the specified row, subtract stars from the specified row, and concatenate with elements after the row
    take (row - 1) board ++ [board !! (row - 1) - stars] ++ drop row board
--This is a function to check if the game is finished
gameFinished :: Board -> Bool
--If all rows are empty the game is finished
gameFinished = all (== 0)