module FileSystemHandler (createFileSystemHandler) where

import Prelude hiding (readFile)

import Config (SiteConfig(..))
import Data.ByteString.Lazy (readFile)
import Gemini (pathComponents, Request(..), respondSuccess, resolveVirtualPath)
import Handler (Handler(..), Handles, Handle)
import Mime (MimeMapping)
import System.Directory (doesFileExist)

data FileSystemHandler = FileSystemHandler {
    fshRoot :: FilePath,
    fshLogger :: String -> IO (),
    fshMimeMap :: MimeMapping
  }

handles :: FileSystemHandler -> Handles
handles handler (Request _ _ path) =
  let rootDir = fshRoot handler;
  in doesFileExist . resolveVirtualPath rootDir $ path

handle :: FileSystemHandler -> Handle
handle handler (Request _ _ path) = do
  let rootDir = fshRoot handler
  let log = fshLogger handler
  let filePath = resolveVirtualPath rootDir path
  let fileName = last . pathComponents $ path
  let fileNameExtension = dropWhile (/= '.') fileName
  let mimeMap = fshMimeMap handler
  let mimeType = show . mimeMap . read $ fileNameExtension
  log ("Serving file " ++ filePath)
  fileContent <- readFile filePath
  return . respondSuccess mimeType $ fileContent

getHandler :: FileSystemHandler -> Handler
getHandler x = Handler (handles x) (handle x)

createFileSystemHandler :: SiteConfig -> (String -> IO ()) -> Handler
createFileSystemHandler config log =
  getHandler $ FileSystemHandler {
    fshRoot = cfgSiteRoot config,
    fshLogger = log,
    fshMimeMap = cfgMimeMap config
  }

