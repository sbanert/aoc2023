import Data.Char (isDigit, ord)
testFileName = "test.txt"
inputFileName = "input.txt"

partHelper :: [String] -> Int
partHelper [] = 0
partHelper [_] = 0
partHelper [_, _] = 0
partHelper (l1:l2:l3:r) = partNumbersInLine False l2 symbols + partHelper (l2:l3:r) where
  symbols = zipWith3 (\c1 c2 c3 -> isSymbol c1 || isSymbol c2 || isSymbol c3) l1 l2 l3

gearHelper :: [String] -> Int
gearHelper [] = 0
gearHelper [_] = 0
gearHelper [_, _] = 0
gearHelper (l1:l2:l3:r) = gearRatiosInLine gearPositions numbers + gearHelper (l2:l3:r) where
  gearPositions = map fst $ filter ((== '*') . snd) (zip [0 ..] l2)
  numbers = numbersInLine 0 l1 ++ numbersInLine 0 l2 ++ numbersInLine 0 l3

adjacent :: Int -> (Int, Int) -> Bool
adjacent p (p1, p2) = p >= p1 - 1 && p <= p2

gearRatiosInLine :: [Int] -> [(Int, (Int, Int))] -> Int
gearRatiosInLine gs ns = foldl ratiosHelper 0 gs where
  ratiosHelper acc p = if length adjList == 2 then acc + product (map fst adjList) else acc where
    adjList = filter (adjacent p . snd) ns

numbersInLine :: Int -> [Char] -> [(Int, (Int, Int))]
numbersInLine _ [] = []
numbersInLine pos l = ans where
  (l1, l2) = span isDigit l
  next | null l2 = []
       | otherwise = numbersInLine (pos + length l1 + 1) (tail l2)
  ans | null l1 = next
      | otherwise = (read l1, (pos, pos + length l1)):next
  
partNumbersInLine _ [] _ = 0
partNumbersInLine _ _ [] = 0
partNumbersInLine s line symbols = ans where
  (l1, l2) = span isDigit line
  (s1, s2) = splitAt (length l1) symbols
  next | null s2 = 0
       | null l2 = 0
       | otherwise = partNumbersInLine (head s2) (tail l2) (tail s2)
  ans | not (null l1) && (s || any id (take (1 + length l1) symbols)) = read l1 + next
      | otherwise = next

isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c = not (isDigit c)

sumPartNumbers :: String -> Int
sumPartNumbers s = partHelper ls' where
  ls = lines s
  dotline = map (const '.') (head ls)
  ls' = dotline:ls ++ [dotline]

sumGearRatios :: String -> Int
sumGearRatios s = gearHelper ls' where
  ls = lines s
  dotline = map (const '.') (head ls)
  ls' = dotline:ls ++ [dotline]

main = do
  testInput <- readFile testFileName
  mainInput <- readFile inputFileName
  
  putStrLn $ "Part 1 Test: " ++ show (sumPartNumbers testInput)
  putStrLn $ "Part 1 Main: " ++ show (sumPartNumbers mainInput)
  putStrLn $ "Part 2 Test: " ++ show (sumGearRatios testInput)
  putStrLn $ "Part 2 Main: " ++ show (sumGearRatios mainInput)
