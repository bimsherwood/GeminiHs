module Main where

import Config (defaultSiteConfig, certificateStore)
import Dir (parsePath)
import Net (CertificateStore(..), serveTlsRequest)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  serveTlsRequest (certificateStore defaultSiteConfig) (\_ -> putStrLn "Accepted request!")
