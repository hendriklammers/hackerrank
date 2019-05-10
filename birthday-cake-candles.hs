-- input :: [Int]
-- input = [3, 2, 1, 3]

solve :: [Int] -> Int
solve = snd . foldr compareHeights (0, 0)
  where
    compareHeights n (height, count)
        | n == height = (height, count + 1)
        | n > height = (n, 1)
        | otherwise = (height, count)


main :: IO ()
main = interact $ show . solve . tail . map read . words
