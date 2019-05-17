import Data.List

compareLists :: [Int] -> [Int] -> [Int]
compareLists a [] = a
compareLists [] b = b
compareLists xs@(x:_) ys@(y:_)
    | length xs > length ys = xs
    | length xs < length ys = ys
    | otherwise =
        if x < y
            then xs
            else ys

migratoryBirds :: [Int] -> Int
migratoryBirds = head . foldr compareLists [] . group . sort

main :: IO ()
main = interact $ show . migratoryBirds . map read . tail . words
