module Cert (FilePairCertStore(..), loadCertificate) where

import Network.TLS (credentialLoadX509, Credential)

data FilePairCertStore = FilePairCertStore {
    certFilePath :: FilePath,
    keyFilePath :: FilePath
  }

loadCertificate store =
  let certPath = certFilePath store;
      privateKeyPath = keyFilePath store
  in credentialLoadX509 certPath privateKeyPath
