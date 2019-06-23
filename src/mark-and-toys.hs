import Data.List

solve :: Int -> [Int] -> Int
solve n = length . takeWhile (<= n) . scanl1 (+) . sort

main :: IO ()
main = interact $ show . (\(n:xs) -> solve n xs) . map read . tail . words
