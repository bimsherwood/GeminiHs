module Main where

import Cert (loadCertificate)
import Config (defaultSiteConfig, certificateStore)
import Gemini (parseRequest)
import FileSystemHandler (createFileSystemHandler)
import Handler (Handler)
import Net (ConnectionHandler, serveTlsRequest)
import Router (route)

fsHandler :: Handler
fsHandler = createFileSystemHandler defaultSiteConfig putStrLn

handlers :: [Handler]
handlers = [fsHandler]

handleConnection :: ConnectionHandler
handleConnection sockAddr ctxt =
  -- TODO: Read request
  let Just request = parseRequest "gemini://localhost/test.txt";
      routed = route handlers
  in routed request sockAddr ctxt

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  cert <- loadCertificate . certificateStore $ defaultSiteConfig
  case cert of
    Left error -> putStrLn error
    Right cert -> serveTlsRequest cert handleConnection
