import Data.List.Split (splitOn)
import Data.List (sort)
import Data.Char (isSpace)
testFileName = "test.txt"
inputFileName = "input.txt"

parseInput :: String -> [(Int, Int)]
parseInput s = (\[a, b] -> zip a b) (map (map read . tail . words) (lines s))

parseInput' :: String -> (Int, Int)
parseInput' s = (\[a, b] -> (read a, read b)) (map (filter (not . isSpace) . unwords . tail . words) (lines s))

numWinRace :: (Int, Int) -> Int
numWinRace (time, distance) = length $ filter (>distance) [ t * (time - t) | t <- [1 .. time]]

numWins :: String -> Int
numWins = product . map numWinRace . parseInput

numWins' :: String -> Int
numWins' = numWinRace . parseInput'

main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  
  putStrLn $ "Part 1 Test: " ++ show (numWins testInput)
  putStrLn $ "Part 1 Main: " ++ show (numWins mainInput)
  putStrLn $ "Part 2 Test: " ++ show (numWins' testInput)
  putStrLn $ "Part 2 Main: " ++ show (numWins' mainInput)
