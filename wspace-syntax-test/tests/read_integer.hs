main :: IO ()
main = do
  stdin <- getContents
  putStr (show (read stdin :: Integer))
