module Mime (FilenameExtension, MimeMapping, MimeType) where

data FilenameExtension = FilenameExtension String
  deriving (Eq)

instance Show (FilenameExtension) where
  show (FilenameExtension str) = '.':str

instance Read (FilenameExtension) where
  readsPrec _ ('.':xs) = [(FilenameExtension xs, "")]
  readsPrec _ _ = []

data MimeType = MimeType String String
  deriving (Eq)

instance Show (MimeType) where
  show (MimeType a b) = a ++ "/" ++ b

instance Read (MimeType) where
  readsPrec _ str =
    let beforeSlash = takeWhile (/= '/') str
        fromSlash = dropWhile (/= '/') str
        leftEmpty = null beforeSlash
        rightEmpty = null fromSlash
    in if leftEmpty || rightEmpty
      then []
      else [(MimeType beforeSlash (tail fromSlash),"")]

type MimeMapping = FilenameExtension -> MimeType
