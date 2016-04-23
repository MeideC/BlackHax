main = do putStrLn "What is 2 + 2?"
          x <- getLine
          checkAnswer x


checkAnswer :: (IO String a) => a -> a
checkAnswer n = putStrLn "You're weronddsa"