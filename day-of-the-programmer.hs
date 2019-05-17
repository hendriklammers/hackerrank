leap :: Int -> String
leap = (++) "12.09." . show

normal :: Int -> String
normal = (++) "13.09." . show

julian :: Int -> String
julian year =
    if year `mod` 4 == 0
        then leap year
        else normal year

gregorian :: Int -> String
gregorian year
    | year `mod` 400 == 0 = leap year
    | year `mod` 4 == 0 && year `mod` 100 /= 0 = leap year
    | otherwise = normal year

dayOfProgrammer :: Int -> String
dayOfProgrammer year
    | year <= 1917 = julian year
    | year == 1918 = "26.09.1918"
    | year >= 1919 = gregorian year

main :: IO ()
main = interact $ dayOfProgrammer . read
