divide :: Int -> Int -> [Int] -> Int
divide d m s
    | length s < m = 0
    | otherwise = summed + divide d m (tail s)
  where
    summed =
        if sum (take m s) == d
            then 1
            else 0

solve :: [Int] -> Int
solve (n:xs) = divide d m s
  where
    s = take n xs
    (d:m:_) = drop n xs

main :: IO ()
main = interact $ show . solve . map read . words
