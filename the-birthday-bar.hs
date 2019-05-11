divide :: Int -> Int -> [Int] -> Int
divide d m s
    | length s < m = 0
    | sum (take m s) == d = 1 + divide d m (tail s)
    | otherwise = 0 + divide d m (tail s)

solve :: [Int] -> Int
solve (n:xs) = divide d m s
  where
    s = take n xs
    (d:m:_) = drop n xs

main :: IO ()
main = interact $ show . solve . map read . words
