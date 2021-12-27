module Main where

import Cert (loadCertificate)
import Config (defaultSiteConfig, certificateStore)
import Data (parsePath)
import FileSystemHandler (createFileSystemHandler)
import Handler (Handler)
import Net (ConnectionHandler, serveTlsRequest)
import Router (route)

fsHandler :: Handler
fsHandler = createFileSystemHandler defaultSiteConfig

handlers :: [Handler]
handlers = [fsHandler]

handleConnection :: ConnectionHandler
handleConnection sockAddr ctxt =
  -- TODO: Read path from request
  let Just path = parsePath "test.txt";
      routed = route handlers
  in routed path sockAddr ctxt

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  cert <- loadCertificate . certificateStore $ defaultSiteConfig
  case cert of
    Left error -> putStrLn error
    Right cert -> serveTlsRequest cert handleConnection
