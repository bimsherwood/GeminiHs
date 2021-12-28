module Gemini (
  parseRequest,
  Path,
  Request(..),
  resolveVirtualPath,
  respondPermFailure,
  respondSuccess,
  Response,
  serialiseResponse) where

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString as Strict
import qualified Data.ByteString.UTF8 as StrictUTF8
import Data.List (intersperse)
import Data.List.Split (splitOn)
import Network.Simple.TCP.TLS (SockAddr)
import Network.URI (parseURI, URI(..))

data PathType = PathType {isAbsolute :: Bool, isIndex :: Bool}
data Path = Path PathType [String]
data Request = Request SockAddr URI Path
data ResponseMeta = ResponseMeta String
data Status =
  Input |
  Success |
  Redirect |
  TempFailure |
  PermFailure |
  ClientCertRequired
data Response = Response {
    responseStatus :: Status,
    responseMeta :: ResponseMeta,
    responseBody :: Maybe LazyUTF8.ByteString
  }

instance Show (Path) where
  show (Path pathType components) =
    let prefix = if isAbsolute pathType then "/" else "";
        suffix = if isIndex pathType then "/" else "";
        isEmpty = null components
    in if isEmpty
      then prefix
      else (prefix++) . (++suffix) . concat . intersperse "/" $ components

statusCode :: Status -> Int
statusCode Input = 10
statusCode Success = 20
statusCode Redirect = 30
statusCode TempFailure = 40
statusCode PermFailure = 50
statusCode ClientCertRequired = 60

pathComponentValid :: String -> Bool
pathComponentValid str =
  let validChar c = not . elem c $ "/?"
      allValidChars = and . map validChar $ str
      notEmpty = not . null $ str
      permitted = str /= ".."
  in allValidChars && notEmpty && permitted

pathComponentFilter :: String -> Bool
pathComponentFilter = not . null

parsePath :: String -> Maybe Path
parsePath [] = Nothing
parsePath str =
  let isAbsolute = head str == '/'
      isIndex = last str == '/'
      pathType = PathType {isAbsolute = isAbsolute, isIndex = isIndex}
      components = filter pathComponentFilter . splitOn "/" $ str
      isValid = and . map pathComponentValid $ components
  in if isValid
    then Just $ Path pathType components
    else Nothing

validateRequestTerminator :: String -> Maybe String
validateRequestTerminator request =
  let limitedRequest = take 1024 request
      requestLines = splitOn "\r\n" limitedRequest
  in case requestLines of
    (x:xs)  -> Just x
    _       -> Nothing

assertGeminiScheme :: URI -> Maybe ()
assertGeminiScheme uri =
  if uriScheme uri == "gemini:"
    then Just ()
    else Nothing

parseRequest :: SockAddr -> StrictUTF8.ByteString -> Maybe Request
parseRequest sockAddr input = do
  let inputStr = StrictUTF8.toString input
  terminatedRequest <- validateRequestTerminator inputStr
  uri <- parseURI terminatedRequest
  assertGeminiScheme uri
  path <- parsePath . uriPath $ uri
  return $ Request sockAddr uri path

meta :: String -> ResponseMeta
meta = ResponseMeta . take 1024

respondSuccess :: String -> LazyUTF8.ByteString -> Response
respondSuccess msg content = Response {
    responseStatus = Success,
    responseMeta = meta msg,
    responseBody = Just content
  }
  
respondPermFailure :: String -> Response
respondPermFailure msg = Response {
    responseStatus = PermFailure,
    responseMeta = meta msg,
    responseBody = Nothing
  }

serialiseResponse :: Response -> LazyUTF8.ByteString
serialiseResponse response =
  let statusStr = show . statusCode . responseStatus $ response
      ResponseMeta metaStr = responseMeta response
      headerStr = statusStr ++ " " ++ metaStr ++ "\r\n"
      header = LazyUTF8.fromString headerStr
  in case responseBody response of
    Just content  -> header <> content
    Nothing       -> header

resolveVirtualPath :: FilePath -> Path -> FilePath
resolveVirtualPath root (Path pathType components) =
  let join = if last root == '/' then "" else "/";
      relativePath = Path (pathType { isAbsolute = False }) components
  in root ++ join ++ show relativePath
