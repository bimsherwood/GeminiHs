module Main where

import Cert (loadCertificate)
import Config (defaultSiteConfig, certificateStore)
import Gemini (parseRequest, serialiseResponse)
import FileSystemHandler (createFileSystemHandler)
import Handler (Handler)
import Net (ConnectionHandler, serveTlsRequest)
import Network.TLS (sendData)
import Router (route)

fsHandler :: Handler
fsHandler = createFileSystemHandler defaultSiteConfig putStrLn

handlers :: [Handler]
handlers = [fsHandler]

handleConnection :: ConnectionHandler
handleConnection sockAddr ctxt =
  -- TODO: Read request
  let Just request = parseRequest undefined "gemini://localhost/test.txt"
      sendResponse = sendData ctxt . serialiseResponse
      routeRequest = route handlers request
  in routeRequest >>= sendResponse

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  cert <- loadCertificate . certificateStore $ defaultSiteConfig
  case cert of
    Left error -> putStrLn error
    Right cert -> serveTlsRequest cert handleConnection
