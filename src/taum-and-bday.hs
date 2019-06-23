import Control.Monad

getInts :: IO [Int]
getInts = map read . words <$> getLine

minCost :: Int -> Int -> Int -> Int -> Int -> Int
minCost b w bc wc z = b * bc' + w * wc'
  where
    bc' =
        if bc <= wc + z
            then bc
            else wc + z
    wc' =
        if wc <= bc + z
            then wc
            else bc + z

solve :: IO Int
solve = do
    [b, w] <- getInts
    [bc, wc, z] <- getInts
    return $ minCost b w bc wc z

main :: IO ()
main = do
    n <- read <$> getLine
    sx <- replicateM n solve
    putStrLn $ unlines $ map show sx
