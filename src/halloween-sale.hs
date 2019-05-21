solve :: [Int] -> Int
solve [p, d, m, s]
    | p > s = 0
    | (p + second) > s && (p + second) `quot` s < 2 = 1
    | total <= 0 = 0
    | otherwise = total
  where
    second = p - d
    first = [p,second .. m]
    total = (s - sum first) `quot` m + length first

main :: IO ()
main = interact $ show . solve . map read . words
