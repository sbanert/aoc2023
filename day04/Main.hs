import Data.List.Split (splitOn)
import Data.List (sort)
testFileName = "test.txt"
inputFileName = "input.txt"

getCards :: String -> [([Int], [Int])]
getCards s = map (cardHelper . words) (lines s)

cardHelper :: [String] -> ([Int], [Int])
cardHelper s = (map read (head numberLists), map read (head (tail numberLists))) where
  numberLists = splitOn ["|"] (tail (tail s))

sortCard :: ([Int], [Int]) -> ([Int], [Int])
sortCard (as, bs) = (sort as, sort bs)

commonNumbers :: ([Int], [Int]) -> Int
commonNumbers ([], _) = 0
commonNumbers (_, []) = 0
commonNumbers (a:as, b:bs) | a == b = 1 + commonNumbers (as, bs)
                           | a > b = commonNumbers (a:as, bs)
                           | a < b = commonNumbers (as, b:bs)

cnToPoints :: Int -> Int
cnToPoints 0 = 0
cnToPoints n = 2^(n-1)

sumPoints :: String -> Int
sumPoints s = sum $ map (cnToPoints . commonNumbers . sortCard) (getCards s)

numCards :: String -> Int
numCards s = numHelper [1, 1 ..] $ map (commonNumbers . sortCard) (getCards s)

numHelper :: [Int] -> [Int] -> Int
numHelper _ [] = 0
numHelper (n:ns) (c:cs) = n + numHelper (zipWith (+) (replicate c n ++ [0, 0 ..]) ns) cs

main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  
  putStrLn $ "Part 1 Test: " ++ show (sumPoints testInput)
  putStrLn $ "Part 1 Main: " ++ show (sumPoints mainInput)
  putStrLn $ "Part 2 Test: " ++ show (numCards testInput)
  putStrLn $ "Part 2 Main: " ++ show (numCards mainInput)
