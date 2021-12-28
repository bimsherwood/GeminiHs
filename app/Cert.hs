module Cert (
  CertificateLoader,
  FilePairCertStore(..),
  loadCertificateFromFiles) where

import Network.TLS (credentialLoadX509, Credential)

type CertificateLoader = IO (Maybe Credential)

data FilePairCertStore = FilePairCertStore {
    certFilePath :: FilePath,
    keyFilePath :: FilePath
  }

loadCertificateFromFiles :: FilePairCertStore -> CertificateLoader
loadCertificateFromFiles store =
  let certPath = certFilePath store
      privateKeyPath = keyFilePath store
  in do
    result <- credentialLoadX509 certPath privateKeyPath
    return $ case result of
      Right cert  -> Just cert
      _           -> Nothing
