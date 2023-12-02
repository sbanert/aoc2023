import Text.Parsec.String (GenParser)
import Text.Parsec (many, many1, (<|>), parse, sepBy)
import Text.Parsec.Char (endOfLine, char, digit, letter, string)

testFileName = "test.txt"
-- testFileName' = "test2.txt"
inputFileName = "input.txt"

data Color = Red | Blue | Green deriving Eq

input :: GenParser Char st [(Int, [[(Int, Color)]])]
input = many game where
  game = do
    string "Game "
    n <- many digit
    string ": "
    ds <- drawing `sepBy` string "; "
    endOfLine
    return (read n, ds)
  drawing = do colour `sepBy` string ", "
  colour = do
    cnum <- many digit
    char ' '
    col <- many letter
    return (read cnum, readColor col)

sumPossibleGames :: [(Int, [[(Int, Color)]])] -> Int
sumPossibleGames gl = sum $ map fst (filter (possibleGame . snd) gl)

sumPowers :: [(Int, [[(Int, Color)]])] -> Int
sumPowers gl = sum $ map (power . concat . snd) gl

power :: [(Int, Color)] -> Int
power l = reds * greens * blues where
  reds = maximum (map fst (filter ((== Red) . snd) l))
  blues = maximum (map fst (filter ((== Blue) . snd) l))
  greens = maximum (map fst (filter ((== Green) . snd) l))

possibleGame :: [[(Int, Color)]] -> Bool
possibleGame gs = not (any impossibleColor (concat gs))

impossibleColor :: (Int, Color) -> Bool
impossibleColor (n, Red) = n > 12
impossibleColor (n, Green) = n > 13
impossibleColor (n, Blue) = n > 14

readColor :: String -> Color
readColor "red" = Red
readColor "blue" = Blue
readColor "green" = Green
readColor _ = undefined

main = do
  testInput <- readFile testFileName
  -- testInput' <- readFile testFileName'
  mainInput <- readFile inputFileName
  
  putStrLn $ "Part 1 Test: " ++ show (sumPossibleGames <$> parse input "Error" testInput)
  putStrLn $ "Part 1 Main: " ++ show (sumPossibleGames <$> parse input "Error" mainInput)
  putStrLn $ "Part 2 Test: " ++ show (sumPowers <$> parse input "Error" testInput)
  putStrLn $ "Part 2 Main: " ++ show (sumPowers <$> parse input "Error" mainInput)
