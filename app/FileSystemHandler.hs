module FileSystemHandler (createFileSystemHandler) where

import Prelude hiding (readFile)

import Config (SiteConfig(..))
import Gemini (Request(..), resolveVirtualPath)
import Data.ByteString.Lazy (readFile)
import Handler (Handler(..), Handles, Handle)
import System.Directory (doesFileExist)
import Network.TLS (sendData)

data FileSystemHandler = FileSystemHandler {
    fshRoot :: FilePath,
    fshLogger :: String -> IO ()
  }

handles :: FileSystemHandler -> Handles
handles handler (Request _ path) _ =
  let rootDir = fshRoot handler;
  in doesFileExist . resolveVirtualPath rootDir $ path

handle :: FileSystemHandler -> Handle
handle handler (Request _ path)  _ context = do
  let rootDir = fshRoot handler
  let log = fshLogger handler
  let filePath = resolveVirtualPath rootDir path
  log ("Serving file " ++ filePath)
  fileContent <- readFile filePath
  sendData context fileContent

getHandler :: FileSystemHandler -> Handler
getHandler x = Handler (handles x) (handle x)

createFileSystemHandler :: SiteConfig a -> (String -> IO ()) -> Handler
createFileSystemHandler config log =
  getHandler $ FileSystemHandler {
    fshRoot = siteRoot config,
    fshLogger = log
  }

