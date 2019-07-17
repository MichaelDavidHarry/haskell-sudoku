import Data.Int
import Data.List

--main = putStr $ printPuzzle puzzle
main = putStr (printPuzzle puzzle)

data SudokuNumber =  One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq, Enum)
data SudokuSquare = Unsolved | Given SudokuNumber | Solved SudokuNumber deriving (Show, Eq)

sudokuNumberFromInt :: Int -> Maybe SudokuNumber 
sudokuNumberFromInt n
    | n > 0 && n < 10 = Just $ toEnum $ n - 1 
    | otherwise = Nothing

intFromSudokuNumber :: SudokuNumber -> Int
intFromSudokuNumber n = fromEnum n + 1

printPuzzle :: [[SudokuSquare]] -> String
printPuzzle [] = ""
printPuzzle (row:rows) = (intercalate "" (map printSudokuSquare row)) ++ "\n" ++ printPuzzle rows

printSudokuSquare square = case square of
    Unsolved -> "-"
    Given n -> show $ intFromSudokuNumber n
    Solved n -> show $ intFromSudokuNumber n

puzzle = [
    [Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Given Three, Given Six, Unsolved],
    [Unsolved, Unsolved, Given Four, Given Six, Unsolved, Given Seven, Unsolved, Given Five, Given Two],
    [Unsolved, Given Five, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Given Eight],
    [Unsolved, Given Four, Unsolved, Unsolved, Given Three, Unsolved, Unsolved, Given Nine, Unsolved],
    [Unsolved, Unsolved, Unsolved, Given Eight, Given One, Given Nine, Unsolved, Unsolved, Unsolved],
    [Unsolved, Given Six, Unsolved, Unsolved, Given Five, Unsolved, Unsolved, Given Eight, Unsolved],
    [Given Three, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Given Four, Unsolved],
    [Given Seven, Given Nine, Unsolved, Given Four, Unsolved, Given One, Given Two, Unsolved, Unsolved],
    [Unsolved, Given Eight, Given Six, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved, Unsolved]
    ]