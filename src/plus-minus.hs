checkNum [x, y, z] n
    | n > 0 = [x + 1, y, z]
    | n < 0 = [x, y + 1, z]
    | otherwise = [x, y, z + 1]

plusMinus (x:xs) = map (show . (/ x)) $ foldl checkNum [0, 0, 0] xs

main = interact $ unlines . plusMinus . map read . words
