import Control.Monad

getInts :: IO [Int]
getInts = map read . words <$> getLine

main :: IO ()
main = do
    [b:_, keyboards, drives] <- replicateM 3 getInts
    case [a | k <- keyboards, d <- drives, let a = k + d, a <= b] of
        [] -> print $ -1
        as -> print $ maximum as
