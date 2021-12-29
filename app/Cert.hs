module Cert (
  Certificate,
  CertificateLoader,
  FilePairCertStore(..),
  loadCertificateFromFiles) where

import Exception (catch)
import Network.TLS (credentialLoadX509, Credential)

type Certificate = Credential
type CertificateLoader = IO (Either String Certificate)

data FilePairCertStore = FilePairCertStore {
    certFilePath :: FilePath,
    keyFilePath :: FilePath
  }

loadCertificateFromFiles :: FilePairCertStore -> CertificateLoader
loadCertificateFromFiles store =
  let certPath = certFilePath store
      privateKeyPath = keyFilePath store
      loadCert = credentialLoadX509 certPath privateKeyPath
      replaceErrorMessage ex =
        Left "Failed to load server certificate from file."
  in fmap (either replaceErrorMessage Right) loadCert 
