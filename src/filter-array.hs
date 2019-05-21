main =
    interact $
    unlines .
    map show .
    (\(x:xs) -> filter (< x) xs) .
    map (read :: String -> Int) .
    words
