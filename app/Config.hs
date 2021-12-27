module Config (
    defaultSiteConfig,
    SiteConfig(..)
  ) where

import Cert (FilePairCertStore(..))

data SiteConfig a = SiteConfig {
    siteRoot :: FilePath,
    certificateStore :: a
  }

defaultCertStore :: FilePairCertStore
defaultCertStore = FilePairCertStore {
    certFilePath = "config/tls.crt",
    keyFilePath = "config/tls.key"
  }

defaultSiteConfig :: SiteConfig FilePairCertStore
defaultSiteConfig = SiteConfig {
    siteRoot = "gemini",
    certificateStore = defaultCertStore
  }