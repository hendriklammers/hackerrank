import Control.Monad

numToString :: Int -> String
numToString n
    | n < 20 =
        [ "one"
        , "two"
        , "three"
        , "four"
        , "five"
        , "six"
        , "seven"
        , "eight"
        , "nine"
        , "ten"
        , "eleven"
        , "twelve"
        , "thirteen"
        , "fourteen"
        , "fifteen"
        , "sixteen"
        , "seventeen"
        , "eighteen"
        , "nineteen"
        ] !!
        (n - 1)
    | otherwise =
        decimals !! ((n `quot` 10) - 2) ++ " " ++ numToString (n `mod` 10)
  where
    decimals =
        [ "twenty"
        , "thirty"
        , "fourty"
        , "fifty"
        , "sixty"
        , "seventy"
        , "eighty"
        , "ninety"
        ]

timeInWords :: Int -> Int -> String
timeInWords h m
    | m == 0 = numToString h ++ " o' clock"
    | m == 1 = "one minute" ++ hour
    | m == 15 = "quarter" ++ hour
    | m < 30 = numToString m ++ " minutes" ++ hour
    | m == 30 = "half" ++ hour
    | m == 45 = "quarter" ++ hour
    | otherwise = numToString (60 - m) ++ " minutes" ++ hour
  where
    hour =
        if m <= 30
            then " past " ++ numToString h
            else " to " ++ numToString (h + 1)

main :: IO ()
main = do
    [h, m] <- replicateM 2 $ fmap read getLine
    putStrLn $ timeInWords h m
