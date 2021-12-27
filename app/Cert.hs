module Cert (FilePairCertStore(..)) where

import Class (CertificateStore(..))
import Network.TLS (credentialLoadX509, Credential)

data FilePairCertStore = FilePairCertStore {
    certFilePath :: FilePath,
    keyFilePath :: FilePath
  }

instance CertificateStore (FilePairCertStore) where
  loadCertificate store =
    let certPath = certFilePath store;
        privateKeyPath = keyFilePath store
    in credentialLoadX509 certPath privateKeyPath
