module Main where

import Net (CertificateStore(..), serveTlsRequest)
import Dir (parsePath)

credStore :: CertificateStore
credStore = FilePairStore "./localhost.crt" "./localhost.key"

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print (parsePath "")
  print (parsePath "/")
  print (parsePath "/a/")
  print (parsePath "/a/b")
  print (parsePath "/../b")
  serveTlsRequest credStore (\_ -> putStrLn "Accepted request!")
