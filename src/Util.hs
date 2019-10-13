module Util (openEditor, getDataPath) where

import Data.ByteString as B
import System.IO
import System.IO.Temp
import System.Process
import System.Exit
import System.Info
import System.FilePath

getDataPath :: FilePath -> String
getDataPath userHomeDir = 
    case os of
        "darwin" -> joinPath [userHomeDir, "Library/Application Support/watson"]
        _ -> error $ "Sorry OS not supported: " ++ os


openEditor :: ByteString -> IO (Maybe ByteString)
openEditor fileContents = do
    let fnameTemplate = "watson-temp.json"
    (resultContents, exitCode) <- internalOpenEditor fnameTemplate fileContents

    -- vim exit codes don't seem to be well documented, 
    -- so while this would be preferred, instead we'll just
    -- do a direct comparison to see if changes were made
    -- case exitCode of
    --     ExitSuccess -> pure $ Just resultContents
    --     ExitFailure i -> pure Nothing

    if fileContents == resultContents then
        pure Nothing
    else
        pure $ Just resultContents

internalOpenEditor :: String -> ByteString -> IO (ByteString, ExitCode)
internalOpenEditor fnameTemplate fileContents = do
    withSystemTempFile fnameTemplate runEditor

    where
        runEditor filePath hdl = do
            hSetBinaryMode hdl True
            hSetBuffering hdl NoBuffering
            B.hPut hdl fileContents
            hClose hdl

            editor <- spawnProcess editorName [filePath]
            exitCode <- waitForProcess editor
            fileResults <- B.readFile filePath

            pure (fileResults, exitCode)

editorName = "vim"
