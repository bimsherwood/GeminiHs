module Gemini (
  parseRequest,
  Path,
  Request(..),
  resolveVirtualPath) where

import Data.List (intersperse)
import Data.List.Split (splitOn)
import Network.URI (parseURI, URI(..))

data PathType = PathType {isAbsolute :: Bool, isIndex :: Bool}
data Path = Path PathType [String]
data Request = Request URI Path

instance Show (Path) where
  show (Path pathType components) =
    let prefix = if isAbsolute pathType then "/" else "";
        suffix = if isIndex pathType then "/" else "";
        isEmpty = null components
    in if isEmpty
      then prefix
      else (prefix++) . (++suffix) . concat . intersperse "/" $ components

splitOnce :: String -> String -> Maybe (String, String)
splitOnce "" str = Just ("", str)
splitOnce _ "" = Nothing
splitOnce substr str@(x:xs) =
  let hasPrefix pref str = take (length pref) str == pref
  in if hasPrefix substr str
    then Just ("", str)
    else splitOnce substr xs >>= \(left, right) -> Just (x:left, right)

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

parseRequest :: String -> Maybe Request
parseRequest str = do
  terminatedRequest <- validateRequestTerminator str
  uri <- parseURI terminatedRequest
  assertGeminiScheme uri
  path <- parsePath . uriPath $ uri
  return $ Request uri path

resolveVirtualPath :: FilePath -> Path -> FilePath
resolveVirtualPath root (Path pathType components) =
  let join = if last root == '/' then "" else "/";
      relativePath = Path (pathType { isAbsolute = False }) components
  in root ++ join ++ show relativePath
