-- input :: [[Int]]
-- input = [[11, 2, 4], [4, 5, 6], [10, 8, -12]]

solve :: [[Int]] -> Int
solve xs = abs $ sum' xs - sum' (reverse xs)
  where
    sum' =
        snd .
        foldl (\(i, total) nums -> (i + 1, total + head (drop i nums))) (0, 0)

main :: IO ()
main = interact $ show . solve . map (map read . words) . tail . lines
