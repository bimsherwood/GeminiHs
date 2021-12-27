module Net (serveTlsRequest) where

import Data.Default.Class
import Data.X509 (SignedCertificate)
import Network.TLS (
  Shared(..),
  ServerHooks,
  Supported,
  DebugParams,
  ServerParams(..),
  Context)
import Network.Simple.TCP.TLS (
  SockAddr,
  HostPreference(..),
  ServiceName,
  serve)

earlyDataSize :: Int
earlyDataSize = 0

ticketLifetime :: Int
ticketLifetime = 86400 -- 86400 seconds = 1 day

sharedParams :: Shared
sharedParams = Shared {
    sharedCredentials = undefined,
    sharedSessionManager = undefined,
    sharedCAStore = undefined,
    sharedValidationCache = undefined,
    sharedHelloExtensions = undefined
  }

serverParams :: ServerParams
serverParams = ServerParams {
    serverWantClientCert = False,
    serverCACertificates = [],
    serverDHEParams = Nothing,
    serverShared = sharedParams,
    serverHooks = def,
    serverSupported = def,
    serverDebug = def,
    serverEarlyDataSize = earlyDataSize,
    serverTicketLifetime = ticketLifetime
  }

hostPreference :: HostPreference
hostPreference = HostAny

port :: ServiceName
port = "1965"

type Handler = (Context, SockAddr) -> IO ()

serveTlsRequest :: Handler -> IO ()
serveTlsRequest handler = serve serverParams hostPreference port handler
