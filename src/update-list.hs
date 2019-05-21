main = interact $ unlines . map (show . abs . read) . words
