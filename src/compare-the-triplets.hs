import Control.Monad

compareScores :: (Int, Int) -> (Int, Int) -> (Int, Int)
compareScores (as, bs) (a, b)
    | a > b = (as + 1, bs)
    | a < b = (as, bs + 1)
    | otherwise = (as, bs)

main :: IO ()
main = do
    [a,b] <- replicateM 2 $ fmap (map (read :: String -> Int) . words) getLine
    let scores = foldl compareScores (0, 0) $ zip a b
    putStrLn $ show (fst scores) ++ " " ++ show (snd scores)
