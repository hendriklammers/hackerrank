classCanceled :: Int -> [Int] -> String
classCanceled n xs =
    if length (filter (<= 0) xs) >= n
        then "NO"
        else "YES"

solve :: [Int] -> [String]
solve [] = []
solve [_] = []
solve (n:k:xs) = classCanceled k (take n xs) : solve (drop n xs)

main :: IO ()
main = interact $ unlines . solve . map read . tail . words
