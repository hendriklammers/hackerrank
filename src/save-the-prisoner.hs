solve :: [Int] -> Int
solve [n, m, s]
    | start >= m = (s - 1) + m
    | rest == 0 = n
    | otherwise = rest
  where
    start = n - (s - 1)
    rest = (m - start) `mod` n

main :: IO ()
main = interact $ unlines . map (show . solve . map read . words) . tail . lines
