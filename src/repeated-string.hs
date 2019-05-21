solve :: String -> Int -> Int
solve s n = countA s * quot n (length s) + countA (take (n `mod` length s) s)
  where
    countA = length . filter (== 'a')

main :: IO ()
main = do
    s <- getLine
    n <- fmap read getLine :: IO Int
    print $ solve s n
