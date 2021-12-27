module Class (CertificateStore(..)) where

import Network.TLS (Credential)

class CertificateStore (a) where
  loadCertificate :: a -> IO (Either String Credential)
