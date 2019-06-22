import Data.List

cutTheSticks :: [Int] -> [Int]
cutTheSticks arr =
    reverse $
    tail $
    foldl (\(x:xs) val -> x - length val : x : xs) [length arr] $
    group $ sort arr

main :: IO ()
main = interact $ unlines . map show . cutTheSticks . map read . tail . words
