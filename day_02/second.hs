import Data.List.Split
import Debug.Trace

magicNumber = 19690720

getProgram :: String -> [Int]
getProgram str = map read $ splitOn "," str

insertNth :: Int -> Int -> [Int] -> [Int]
insertNth n newElement xs = take n xs ++ [newElement] ++ drop (n + 1) xs

executeAt :: Int -> [Int] -> [Int]
executeAt n program
    -- | trace ("a = " ++ show a) False = undefined
    | a == 99 = program
    | a == 1 = executeAt (n+4) $ insertNth d (program!!b + program!!c) program
    | a == 2 = executeAt (n+4) $ insertNth d (program!!b * program!!c) program
  where (a:b:c:d:xs) = drop n program

runProgram :: [Int] -> Int -> Int -> Int
runProgram program noun verb = executeAt 0
                               (insertNth 1 noun $
                                insertNth 2 verb $ program)
                               !! 0

findNounVerb :: [Int] -> (Int, Int)
findNounVerb program = head $ filter
                        (\(n, v) -> runProgram program n v == magicNumber)
                        [(n, v) | n <- [0..99], v <- [0..99]]

main = do
  contents <- readFile "input"
  let program = getProgram contents
  print $ (\(noun, verb) -> 100*noun + verb) $ findNounVerb program
