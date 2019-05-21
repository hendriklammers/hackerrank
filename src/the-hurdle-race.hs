solve :: [Int] -> Int
solve (k:heights)
    | dif > 0 = dif
    | otherwise = 0
  where
    dif = maximum heights - k

main :: IO ()
main = interact $ show . solve . map read . tail . words
