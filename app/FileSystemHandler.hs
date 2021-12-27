module FileSystemHandler (createFileSystemHandler) where

import Prelude hiding (readFile)

import Config (SiteConfig(..))
import Gemini (resolvePath)
import Data.ByteString.Lazy (readFile)
import Handler (Handler(..), Handles, Handle)
import System.Directory (doesFileExist)
import Network.TLS (sendData)

data FileSystemHandler = FileSystemHandler {
    root :: FilePath,
    logger :: String -> IO ()
  }

handles :: FileSystemHandler -> Handles
handles handler path _ =
  let rootDir = root handler;
  in doesFileExist . resolvePath rootDir $ path

handle :: FileSystemHandler -> Handle
handle handler path _ context = do
  let rootDir = root handler
  let filePath = resolvePath rootDir path
  let log = logger handler
  log ("Sending file " ++ filePath)
  fileContent <- readFile filePath
  sendData context fileContent

getHandler :: FileSystemHandler -> Handler
getHandler x = Handler (handles x) (handle x)

createFileSystemHandler :: SiteConfig a -> (String -> IO ()) -> Handler
createFileSystemHandler config log =
  getHandler $ FileSystemHandler {
    root = siteRoot config,
    logger = log
  }

