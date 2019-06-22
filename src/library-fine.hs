import Control.Monad

type Date = (Int, Int, Int)

inputDate :: IO Date
inputDate = stringToDate <$> getLine
  where
    stringToDate = (\[d, m, y] -> (d, m, y)) . map read . words

libraryFine :: Date -> Date -> Int
libraryFine (d1, m1, y1) (d2, m2, y2)
    | y1 > y2 = 10000
    | y1 == y2 && m1 > m2 = (m1 - m2) * 500
    | y1 == y2 && m1 == m2 && d1 > d2 = (d1 - d2) * 15
    | otherwise = 0

main :: IO ()
main = do
    [d1, d2] <- replicateM 2 inputDate
    print $ libraryFine d1 d2
