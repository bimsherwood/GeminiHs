module Main where

import Net (CertificateStore(..), serveTlsRequest)

credStore :: CertificateStore
credStore = FilePairStore "./localhost.crt" "./localhost.key"

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  serveTlsRequest credStore (\_ -> putStrLn "Accepted request!")
