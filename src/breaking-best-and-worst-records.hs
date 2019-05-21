accScores :: [[Int]] -> Int -> [[Int]]
accScores [x:xs, m:ms] score = [max, min]
  where
    max =
        if score > x
            then score : x : xs
            else x : xs
    min =
        if score < m
            then score : m : ms
            else m : ms

solve :: [Int] -> [Int]
solve (x:xs) = map (length . tail) $ foldl accScores [[x], [x]] xs

main :: IO ()
main = interact $ unwords . map show . solve . tail . map read . words
