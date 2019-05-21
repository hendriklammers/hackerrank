removeOdds :: [Int] -> [Int]
removeOdds [] = []
removeOdds [_] = []
removeOdds (_:x:xs) = x : removeOdds xs

main =
    interact $ unlines . map show . removeOdds . map read . words
