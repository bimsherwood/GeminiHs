module Net (CertificateStore(..), serveTlsRequest) where

import Class (CertificateStore(..))
import Data.Default.Class
import Data.X509 (SignedCertificate)
import Network.TLS (
  Context,
  Credential,
  Credentials(..),
  DebugParams,
  ServerHooks,
  ServerParams(..),
  Shared(..),
  Supported(..),
  Version(..))
import Network.TLS.Extra.Cipher (
  ciphersuite_default)
import Network.Simple.TCP.TLS (
  HostPreference(..),
  serve,
  ServiceName,
  SockAddr)

type Handler = (Context, SockAddr) -> IO ()

earlyDataSize :: Int
earlyDataSize = 0

ticketLifetime :: Int
ticketLifetime = 86400 -- 86400 seconds = 1 day

hostPreference :: HostPreference
hostPreference = HostAny

port :: ServiceName
port = "1965"

sharedParams :: Credentials -> Shared
sharedParams creds = def {
    sharedCredentials = creds
  }

supported :: Supported
supported = def {
    supportedVersions = [TLS13, TLS12],
    supportedCiphers = ciphersuite_default
  }

serverParams :: Credentials -> ServerParams
serverParams creds = def {
    serverShared = sharedParams creds,
    serverSupported = supported,
    serverEarlyDataSize = earlyDataSize,
    serverTicketLifetime = ticketLifetime
  }

serveTlsRequest :: CertificateStore a => a -> Handler -> IO ()
serveTlsRequest certStore handler = do
  loadCertResult <- loadCertificate certStore
  case loadCertResult of
    Left error -> putStrLn error
    Right cred ->
      let params = serverParams (Credentials [cred])
      in serve params hostPreference port handler
