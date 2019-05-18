reverseInt :: Int -> Int
reverseInt = read . reverse . show

solve :: [Int] -> Int
solve [i, j, k] =
    length
        [dx | x <- [i .. j], let dx = abs (x - reverseInt x), dx `mod` k == 0]

main :: IO ()
main = interact $ show . solve . map read . words
