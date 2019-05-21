solve :: [Int] -> Int
solve (k:ar) =
    length
        [ (x, y)
        | x <- zip [1 ..] ar
        , y <- zip [1 ..] ar
        , fst x < fst y
        , (snd x + snd y) `mod` k == 0
        ]

main :: IO ()
main = interact $ show . solve . map read . tail . words
