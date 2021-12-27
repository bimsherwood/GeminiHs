module FileSystemHandler (createFileSystemHandler) where

import Config (SiteConfig(..))
import Data (resolvePath)
import Handler (Handler(..))
import System.Directory (doesFileExist)

data FileSystemHandler = FileSystemHandler { root :: FilePath }

handles handler path _ =
  let rootDir = root handler;
  in doesFileExist . resolvePath rootDir $ path

handle handler path _ context =
  putStrLn $ "Fake handling path: " ++ show path
  -- TODO: writeBytes (resolvePath (root handler) path) context

getHandler :: FileSystemHandler -> Handler
getHandler x = Handler (handles x) (handle x)

createFileSystemHandler :: SiteConfig a -> Handler
createFileSystemHandler config =
  getHandler $ FileSystemHandler { root = siteRoot config }

