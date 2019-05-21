import Control.Monad
import Data.List

isFunction :: [(Int, Int)] -> String
isFunction [] = "YES"
isFunction [_] = "YES"
isFunction (x:y:xs) =
    if fst x == fst y && snd x /= snd y
        then "NO"
        else isFunction $ y : xs

toPair :: String -> (Int, Int)
toPair = (\(x:y:_) -> (x, y)) . map read . words

getCase :: IO String
getCase = do
    n <- read <$> getLine
    xs <- replicateM n $ toPair <$> getLine
    return $ isFunction $ sortOn fst xs

main :: IO ()
main = do
    n <- read <$> getLine
    cases <- replicateM n getCase
    putStrLn $ unlines cases
