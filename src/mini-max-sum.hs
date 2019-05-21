import Data.List

solve :: [Int] -> [Int]
solve xs = [sum $ take 4 sorted, sum $ drop 1 sorted]
  where
    sorted = sort xs

main :: IO ()
main = interact $ unwords . map show . solve . map read . words
