solve :: [Int] -> Int
solve (d:xs) =
    length
        [ (i, j, k)
        | i <- xs
        , j <- xs
        , k <- xs
        , i < j && j < k
        , j - i == d && k - j == d
        ]

main :: IO ()
main = interact $ show . solve . tail . map read . words
