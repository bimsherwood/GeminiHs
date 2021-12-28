module Main where

import Cert (loadCertificate)
import Config (defaultSiteConfig, certificateStore, SiteConfig)
import Gemini (
  parseRequest,
  serialiseResponse,
  Request,
  respondPermFailure,
  Response)
import FileSystemHandler (createFileSystemHandler)
import Handler (Handle, Handler, route)
import Net (ConnectionHandler, serveTlsRequest)
import Network.TLS (recvData, sendData)
import NotFoundHandler (createNotFoundHandler)

logMsg :: String -> IO ()
logMsg = putStrLn

invalidRequestResponse :: IO Response
invalidRequestResponse = do
  logMsg "Error: Invalid request."
  return . respondPermFailure $ "Invalid request."

noHandlerResponse :: Request -> IO Response
noHandlerResponse _ = do
  logMsg "Error: No appropriate handler was found."
  return . respondPermFailure $
    "No appropriate handler was found for this request."

createRouter :: SiteConfig a -> Handle
createRouter config = route noHandlerResponse [
    createFileSystemHandler config logMsg,
    createNotFoundHandler logMsg
  ]

createConnectionHandler :: SiteConfig a -> ConnectionHandler
createConnectionHandler config sockAddr ctxt = do
  let readRequest = parseRequest sockAddr `fmap` recvData ctxt
  let sendResponse = sendData ctxt . serialiseResponse
  let route = createRouter config
  request <- readRequest
  case request of
    Just request  -> route request >>= sendResponse
    Nothing       -> invalidRequestResponse >>= sendResponse

main :: IO ()
main = do
  logMsg "Taking protein pills and putting helmet on"
  let config = defaultSiteConfig
  let connectionHandler = createConnectionHandler config
  cert <- loadCertificate . certificateStore $ config
  case cert of
    Left error -> logMsg error
    Right cert -> serveTlsRequest cert connectionHandler
