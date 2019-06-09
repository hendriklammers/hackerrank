jump :: Int -> [Int] -> Int
jump k xs
    | null xs = 0
    | length xs < k = 1
    | otherwise =
        jump k (drop k xs) +
        if head xs == 1
            then 3
            else 1

getInts :: IO [Int]
getInts = map read . words <$> getLine

main :: IO ()
main = do
    [_, k] <- getInts
    clouds <- getInts
    print $ 100 - jump k clouds
