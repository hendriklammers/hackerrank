sumOdds :: [Int] -> Int
sumOdds = sum . filter (\x -> mod x 2 /= 0)
-- sumOdds = foldl (\acc x -> acc + (if mod x 2 /= 0 then x else 0)) 0

main :: IO ()
main =
    interact $ show . sumOdds . map read . words
