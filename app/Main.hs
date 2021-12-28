module Main where

import Cert (CertificateLoader)
import Config (defaultSiteConfig, SiteConfig(..))
import DefaultDocumentHandler (createDefaultDocumentHandler)
import Gemini (
  parseRequest,
  serialiseResponse,
  Request,
  respondPermFailure,
  Response)
import FileSystemHandler (createFileSystemHandler)
import Handler (Handler, route, Router)
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

createRouter :: SiteConfig -> Router
createRouter config = route noHandlerResponse [
    createFileSystemHandler config logMsg,
    createDefaultDocumentHandler config logMsg,
    createNotFoundHandler logMsg
  ]

createConnectionHandler :: Router -> ConnectionHandler
createConnectionHandler route sockAddr ctxt = do
  let readRequest = parseRequest sockAddr `fmap` recvData ctxt
  let sendResponse = sendData ctxt . serialiseResponse
  request <- readRequest
  case request of
    Just request  -> route request >>= sendResponse
    Nothing       -> invalidRequestResponse >>= sendResponse

main :: IO ()
main = do
  logMsg "Taking protein pills and putting helmet on"
  let config = defaultSiteConfig
  let router = createRouter config
  let connectionHandler = createConnectionHandler router
  cert <- cfgLoadServerCert config
  case cert of
    Just cert -> serveTlsRequest cert connectionHandler
    Nothing   -> logMsg "Failed to load server certificate."
