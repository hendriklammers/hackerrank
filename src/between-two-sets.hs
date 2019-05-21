import Control.Monad

getInts :: IO [Int]
getInts = map read . words <$> getLine

main :: IO ()
main = do
    [_, as, bs] <- replicateM 3 getInts
    let aLcm = foldl1 lcm as
        bGcd = foldl1 gcd bs
    print
        $ length
        $ filter ((0 ==) . mod bGcd)
        $ takeWhile (<= bGcd)
        $ map (* aLcm) [1 ..]
