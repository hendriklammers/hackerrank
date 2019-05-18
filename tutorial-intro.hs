import Data.List
import Data.Maybe

solve :: [Int] -> Int
solve (n:_:xs) =
    fromMaybe (-1) $ elemIndex n xs

main :: IO ()
main = interact $ show . solve . map read . words
