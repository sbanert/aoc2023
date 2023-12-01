import Data.Char (isDigit, ord)
testFileName = "test.txt"
testFileName' = "test2.txt"
inputFileName = "input.txt"

calHelper :: Maybe (Int, Int) -> Char -> Maybe (Int, Int)
calHelper Nothing c = if isDigit c then Just (ord c - ord '0', ord c - ord '0')
                                 else Nothing
calHelper (Just (n1, n2)) c = if isDigit c then Just (n1, ord c - ord '0') else Just (n1, n2)

calValue' :: String -> Int
calValue' = numberFromDigits . calHelper' Nothing

calHelper' :: Maybe (Int, Int) -> String -> Maybe (Int, Int)
calHelper' Nothing [] = Nothing
calHelper' Nothing ('o':'n':'e':r) = calHelper' (Just (1, 1)) ('e':r)
calHelper' Nothing ('t':'w':'o':r) = calHelper' (Just (2, 2)) ('o':r)
calHelper' Nothing ('t':'h':'r':'e':'e':r) = calHelper' (Just (3, 3)) ('e':r)
calHelper' Nothing ('f':'o':'u':'r':r) = calHelper' (Just (4, 4)) r
calHelper' Nothing ('f':'i':'v':'e':r) = calHelper' (Just (5, 5)) ('e':r)
calHelper' Nothing ('s':'i':'x':r) = calHelper' (Just (6, 6)) r
calHelper' Nothing ('s':'e':'v':'e':'n':r) = calHelper' (Just (7, 7)) ('n':r)
calHelper' Nothing ('e':'i':'g':'h':'t':r) = calHelper' (Just (8, 8)) ('t':r)
calHelper' Nothing ('n':'i':'n':'e':r) = calHelper' (Just (9, 9)) ('e':r)
calHelper' Nothing (c:r) = if isDigit c
                           then calHelper' (Just (ord c - ord '0', ord c - ord '0')) r
                           else calHelper' Nothing r
calHelper' (Just (n1, n2)) [] = Just (n1, n2)
calHelper' (Just (n1, n2)) ('o':'n':'e':r) = calHelper' (Just (n1, 1)) ('e':r)
calHelper' (Just (n1, n2)) ('t':'w':'o':r) = calHelper' (Just (n1, 2)) ('o':r)
calHelper' (Just (n1, n2)) ('t':'h':'r':'e':'e':r) = calHelper' (Just (n1, 3)) ('e':r)
calHelper' (Just (n1, n2)) ('f':'o':'u':'r':r) = calHelper' (Just (n1, 4)) r
calHelper' (Just (n1, n2)) ('f':'i':'v':'e':r) = calHelper' (Just (n1, 5)) ('e':r)
calHelper' (Just (n1, n2)) ('s':'i':'x':r) = calHelper' (Just (n1, 6)) r
calHelper' (Just (n1, n2)) ('s':'e':'v':'e':'n':r) = calHelper' (Just (n1, 7)) ('n':r)
calHelper' (Just (n1, n2)) ('e':'i':'g':'h':'t':r) = calHelper' (Just (n1, 8)) ('t':r)
calHelper' (Just (n1, n2)) ('n':'i':'n':'e':r) = calHelper' (Just (n1, 9)) ('e':r)
calHelper' (Just (n1, n2)) (c:r) = if isDigit c
                                   then calHelper' (Just (n1, ord c - ord '0')) r
                                   else calHelper' (Just (n1, n2)) r

numberFromDigits :: Maybe (Int, Int) -> Int
numberFromDigits Nothing = undefined
numberFromDigits (Just (n1, n2)) = 10 * n1 + n2

calValue :: String -> Int
calValue = numberFromDigits . foldl calHelper Nothing

sumCalibrationValues :: String -> Int
sumCalibrationValues s = sum (map calValue (lines s))

sumCalibrationValues' :: String -> Int
sumCalibrationValues' s = sum (map calValue' (lines s))

main = do
  testInput <- readFile testFileName
  testInput' <- readFile testFileName'
  mainInput <- readFile inputFileName
  
  putStrLn $ "Part 1 Test: " ++ show (sumCalibrationValues testInput)
  putStrLn $ "Part 1 Main: " ++ show (sumCalibrationValues mainInput)
  putStrLn $ "Part 2 Test: " ++ show (sumCalibrationValues' testInput')
  putStrLn $ "Part 2 Main: " ++ show (sumCalibrationValues' mainInput)
