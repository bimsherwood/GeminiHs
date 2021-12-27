module Dir (Site(..), parsePath, componentValid) where

import Data.List (intersperse)
import Data.List.Split (splitOn)

data Site = Site {
    siteRoot :: FilePath,
    configRoot :: FilePath
  }

data PathType = PathType {isAbsolute :: Bool, isIndex :: Bool}
data Path = Path PathType [String]

instance Show (Path) where
  show (Path pathType components) =
    let prefix = if isAbsolute pathType then "/" else "";
        suffix = if isIndex pathType then "/" else "";
        isEmpty = null components
    in if isEmpty
      then prefix
      else (prefix++) . (++suffix) . concat . intersperse "/" $ components

componentValid :: String -> Bool
componentValid str =
  let validChar c = not . elem c $ "./"
      allValidChars = and . map validChar $ str
      notEmpty = not . null $ str
  in allValidChars && notEmpty

parsePath :: String -> Maybe Path
parsePath [] = Nothing
parsePath str =
  let isAbsolute = head str == '/';
      isIndex = last str == '/';
      pathType = PathType {isAbsolute = isAbsolute, isIndex = isIndex}
      components = filter (not . null) . splitOn "/" $ str;
      isValid = and . map componentValid $ components
  in if isValid
    then Just $ Path pathType components
    else Nothing
