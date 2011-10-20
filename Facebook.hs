module Facebook where

import qualified Data.ByteString.Char8 as BS
import Data.Digest.OpenSSL.MD5
import Control.Concurrent
import qualified Data.List as DL
import qualified Data.Map as DM
import JSON
import Network.HTTP
import Network.URI
import Network.URL
import Util

versionTuple = ("v", "1.0")

facebookHost = Absolute (Host (HTTP False) "api.facebook.com" Nothing)

facebookLogin  = (URL facebookHost "login.php") . (versionTuple:)
facebookLogin' = exportURL . facebookLogin

facebookServer  = (URL facebookHost "restserver.php") . (versionTuple:)
facebookServer' = exportURL . facebookServer

data FBFormat = FBXML | FBJSON

instance Show FBFormat where
    show FBXML = "XML"
    show _     = "JSON"

data Facebook = Facebook {sessionKey, uid, sessionSecret, apiKey :: String,
                                                         expires :: Int,
                                                          format :: FBFormat}

class ParsedResp a where
    parseResp  :: Monad m => String -> m a

type APIKey      = String
type APISecret   = String
type Generator a = SampleVar () -> Facebook -> IO a
type FBToken     = String
type LoginURL    = String

signify :: String -> [(String, String)] -> [(String, String)]
signify sec cles =
    let rzt = versionTuple:cles in
    let dic = DM.fromList rzt in
    let cs' = DL.sort (DM.keys dic) in
    let ps' = pullPairs dic cs' in
    let cct = fbConcat ps' in
    let sig = md5Sum (cct ++ sec) in
        ("sig", sig):cles
    where
    pullPairs :: DM.Map String String -> [String] -> [(String, String)]
    pullPairs dct = foldr (\x y -> case DM.lookup x dct of
        Just z -> (x, z):y
        _      -> y) []

    fbConcat :: [(String, String)] -> String
    fbConcat = foldr (\(x, y) z -> x ++ "=" ++ y ++ z) ""

    md5Sum :: String -> String
    md5Sum = md5sum . BS.pack

createToken :: APIKey -> APISecret -> [(String, String)] -> IO (FBToken, LoginURL)
createToken cle sec prs =
    case parseAbsoluteURI (facebookServer'
        (signify sec [("api_key", cle), ("format", "JSON"),
        ("method", "facebook.auth.createToken")])) of
        Just u -> do
            got <- simpleHTTP (Request u GET [] "")
            case got of
                Left e -> fail ("Connection error: " ++ (show e))
                Right (Response _ _ _ b) -> case toJSON ("[" ++ b ++ "]") of
                    Just (JArr [JStr r]) -> return (r, facebookLogin'
                        ([("auth_token", r), ("api_key", cle), ("popup", "true")] ++ prs))
                    _     -> fail ("Facebook's JSON is terrible: " ++ b)
        _      -> fail "Can't parse URL"

desktopSession :: (ParsedResp p, Monad m) => APIKey -> APISecret -> FBToken ->
    Generator (m p) -> FBFormat -> [(String, String)] -> IO ()
desktopSession cle sec tok gen fmt prs = do
    case parseAbsoluteURI (facebookServer'
        (signify sec ([("api_key", cle), ("format", "JSON"), ("auth_token", tok),
        ("method", "facebook.auth.getSession")] ++ prs))) of
        Just u -> do
            got <- simpleHTTP (Request u GET [] "")
            case got of
                Left e -> fail ("Connection error: " ++ (show e))
                Right (Response _ _ _ b) -> case toJSON b of
                    Just j -> pullInfo j gen tok cle fmt
                    _      -> fail ("Facebook's JSON is terrible: " ++ b)
        _      -> fail "Can't parse URL."
    where
    pullInfo :: (Monad m, ParsedResp p) => JSONValue -> Generator (m p) ->
        FBToken -> APIKey -> FBFormat -> IO ()
    pullInfo j gen tk cle fmt =
        case ("session_key" @@ j, "uid" @@ j, "expires" @@ j, "secret" @@ j) of
            (Just (JStr ses), Just (JNum uidn),
             Just (JNum exp), Just (JStr sec)) -> do
                runSession (Facebook ses (show uidn) sec cle exp fmt) gen
            _   -> fail ("What is this response to getSession? " ++ (show j))

runSession :: (ParsedResp p, Monad m) => Facebook -> Generator (m p)
    -> IO ()
runSession fb gen = do
    sv <- newEmptySampleVar
    gn <- forkIO (runGen gen sv fb)
    readSampleVar sv
    killThread gn
    where

    runGen :: (ParsedResp p, Monad m) => Generator (m p) ->
        SampleVar () -> Facebook -> IO ()
    runGen gen mort fb = do
        got <- gen mort fb
        runGen gen mort fb

execute :: (ParsedResp p, Monad m) => Facebook -> String -> [(String, String)]
    -> IO (m p)
execute (Facebook ses uid sec cle exp frm) mth prs =
    let url = facebookServer' (signify sec ([("method", "facebook." ++ mth),
                               ("api_key", cle),
                               ("format", show frm),
                               ("session_key", ses)] ++ prs)) in
    case parseAbsoluteURI url of
        Just u -> do
            rez <- simpleHTTP (Request u GET [] "")
            case rez of
                Left _                   -> fail "Connection error."
                Right (Response _ _ _ b) -> return (parseResp b)
        _      -> fail "Server URL has problems."

statusSet :: (ParsedResp p, Monad m) => Facebook -> String -> IO (m p)
statusSet fb stt = execute fb "Status.set" [("status", stt)]
