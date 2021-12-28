module Config (
    defaultSiteConfig,
    SiteConfig(..)
  ) where

import Cert (
  CertificateLoader,
  FilePairCertStore(..),
  loadCertificateFromFiles)
import Mime (MimeMapping)

data SiteConfig = SiteConfig {
    cfgSiteRoot :: FilePath,
    cfgDefaultDocument :: Maybe String,
    cfgLoadServerCert :: CertificateLoader,
    cfgMimeMap :: MimeMapping
  }

basicMimeMappings :: [(String, String)]
basicMimeMappings = [
    (".gmi", "text/gemini"),
    (".gemini", "text/gemini"),
    (".txt", "text/plain")
  ]

fallbackMimeMapping :: String
fallbackMimeMapping = "application/octet-stream"

basicMapping :: MimeMapping
basicMapping ext =
  let parseMapping (a,b) = (read a, read b)
      mappings = map parseMapping basicMimeMappings
      matchingMapping = filter ((== ext) . fst) mappings
  in case matchingMapping of
    ((ext, mime):xs)  -> mime
    _       -> read fallbackMimeMapping

defaultCertStore :: FilePairCertStore
defaultCertStore = FilePairCertStore {
    certFilePath = "config/tls.crt",
    keyFilePath = "config/tls.key"
  }

defaultSiteConfig :: SiteConfig
defaultSiteConfig = SiteConfig {
    cfgSiteRoot = "gemini",
    cfgDefaultDocument = Just "index.gmi",
    cfgLoadServerCert = loadCertificateFromFiles defaultCertStore,
    cfgMimeMap = basicMapping
  }