import Data.List

solve :: [Int] -> Int
solve = maximum . map length . groupBy (\x y -> abs (x - y) <= 1) . sort

main :: IO ()
main = interact $ show . solve . map read . tail . words
