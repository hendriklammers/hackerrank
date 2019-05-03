import Control.Monad

main = readLn >>= (`replicateM_` putStrLn "Hello World")
