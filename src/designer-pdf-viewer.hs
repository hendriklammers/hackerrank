letterHeight :: [Int] -> Char -> Int
letterHeight (x:_) 'a' = x
letterHeight hs c = hs !! (length ['a' .. c] - 1)

main :: IO ()
main = do
    h <- fmap (map read . words) getLine :: IO [Int]
    word <- getLine
    print $ (* length word) $ maximum $ map (letterHeight h) word
