import Control.Monad

main :: IO ()
main =
    getContents >>=
    mapM_ print . (\(x:xs) -> (concatMap . replicate) x xs) . map read . words
