module Main where

import Net (serveTlsRequest)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  serveTlsRequest (\_ -> putStrLn "Accepted request!")
