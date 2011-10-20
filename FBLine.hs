module FBLine where

import Facebook
import JSON
import System.Environment
import Util

instance ParsedResp JSONValue where
    parseResp ""      = return JNull
    parseResp d@(x:_) = if or [x == y | y <- "{["] then toJSON d else
        parseResp ("[" ++ d ++ "]")
    
perms :: String
perms = "read_stream,publish_stream,offline_access,status_update"

main :: IO ()
main = do
    apiKey <- getEnv "FBAPIKEY" `catch` (\_ -> do
        putStrLn "Facebook API Key?"
        getLine)
    apiSec <- getEnv "FBAPISEC" `catch` (\_ -> do
        putStrLn "Facebook API Secret?"
        getLine)
    (tk, lu) <- createToken apiKey apiSec [("req_perms", perms)]
    putStrLn ("Visit this URL, and then press Enter:")
    putStrLn lu
    putStrLn ""
    getLine
    desktopSession apiKey apiSec tk generator FBJSON []

generator :: Generator (Success JSONValue)
generator mort fb = do
    putStrLn "FB Status?"
    stt <- statusSet fb =<< getLine
    putStrLn (show stt)
    return stt
