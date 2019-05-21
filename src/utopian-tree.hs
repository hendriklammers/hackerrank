utopianTree :: Int -> Int
utopianTree n
    | n == 0 = 1
    | n `mod` 2 == 0 = 1 + utopianTree (n - 1)
    | otherwise = 2 * utopianTree (n - 1)

main :: IO ()
main = interact $ unlines . map (show . utopianTree . read) . tail . lines
