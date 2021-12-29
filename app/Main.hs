module Main where

import Args (Args(..), loadArgs)
import Cert (Certificate, CertificateLoader)
import Config (defaultSiteConfig, SiteConfig(..))
import Exception (catch, Exception(..), ($?), (??), (<<?))
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
  dataIn <- recvData ctxt
  let request = parseRequest sockAddr dataIn
  response <- fmap succeed . route <<? request
  let sendResponse = sendData ctxt . serialiseResponse
  let handleError _ = invalidRequestResponse >>= sendResponse
  catch handleError (sendResponse $? response)

loadConfiguration :: Args -> IO (Either String SiteConfig)
loadConfiguration (Args filePath) = return . return $ defaultSiteConfig -- TODO!

serve :: Certificate -> ConnectionHandler -> IO ()
serve cert connectionHandler = do
  logMsg "Taking protein pills and putting helmet on."
  serveTlsRequest cert connectionHandler

main :: IO ()
main = do
  args <- loadArgs
  config <- loadConfiguration <<? args
  cert <- cfgLoadServerCert <<? config
  let router = createRouter $? config
  let connectionHandler = createConnectionHandler $? router
  let handleError = either logMsg id
  catch handleError (serve $? cert ?? connectionHandler)
