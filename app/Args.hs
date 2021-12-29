module Args (Args(..), loadArgs) where

import System.Environment (getArgs)

data Args = Args FilePath

errorMessage :: String
errorMessage = "Specify a configuration file."

parseArgs :: [String] -> Either String Args
parseArgs [configFilePath] = Right $ Args configFilePath
parseArgs _ = Left errorMessage

loadArgs :: IO (Either String Args)
loadArgs = fmap parseArgs getArgs
