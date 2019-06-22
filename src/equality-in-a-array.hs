import Data.List

equalizeArray :: [Int] -> Int
equalizeArray arr = length arr - remaining
  where
    remaining = maximum $ map length $ group $ sort arr

main :: IO ()
main = interact $ show . equalizeArray . map read . tail . words
