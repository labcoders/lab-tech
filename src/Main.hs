module Main where

import Control.Concurrent
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.Text as T

import Network
import Network.HTTP

import System.FilePath.Posix
import System.IO
import System.Directory

import Text.Read
import Text.Printf

data Command 
    = Upload 
    | Help
    | List
    deriving (Show, Read)

type Channel = String

name = "lab-tech"
ircServer = "labcoders.club"
ircPort = 6667
generalChan = "#general"
path = "data"

help = 
     [ "Available commands:"
     , "!help -> Print help"
     , "!upload [url] [filename] -> Downloads the provided url to the data directory with the provided filename."
     , "!list -> List all available files downloaded in the data directory."
     ]

write :: Handle -> String -> String -> IO ()
write h a b = do
    hPrintf h "%s %s\r\n" a b
    printf "> %s %s\n" a b

privmsg :: Handle -> Channel -> String -> IO ()
privmsg h c s = hPrintf h "PRIVMSG %s %s\r\n" c s

listen :: Handle -> IO ()
listen h = forever $ do
    s <- hGetLine h
    if ping s then pong h s
    else handle h $ drop 1 s

clean :: String -> String
clean = drop 1 . dropWhile (/= ':')

handle :: Handle -> String -> IO ()
handle h [] = return ()
handle h s = 
    let cleaned = clean s in
        case cleaned of
            [] -> return ()
            _ -> if head cleaned == '!' then do
                    print $ "Command: " ++ cleaned
                    let args = words $ tail cleaned in do
                        if length args == 0 then return ()
                        else case readMaybe $ toCamel (head args) of
                            Just a -> run h a $ tail args
                            Nothing -> print $ "Failed to parse command: " ++ head args
                 else print cleaned

ping :: String -> Bool
ping s = "PING :" `isPrefixOf` s

pong :: Handle -> String -> IO ()
pong h s = write h "PONG" (':' : drop 6 s)

run :: Handle -> Command -> [String] -> IO ()
run h com args = case com of
    Upload -> do
        print "Upload called."
        if length args /= 2 then privmsg h generalChan $ "Incorrect number of arguments to upload."
        else do
            printCommand com args
            let url = args !! 0
                filename = args !! 1
            print $ "Downloading '" ++ url ++ "' and saving to data/" ++ filename
            response <- simpleHTTP $ getRequest url
            case response of
                Left e -> privmsg h generalChan $ "Failed to save " ++ filename ++ " to disk."
                Right b -> do
                    print $ "Response code: " ++ (show $ rspCode b)
                    print $ "Response reason: " ++ (show $ rspReason b)
                    print "Response headers: "
                    mapM (print . show) $ rspHeaders b
                    exists <- doesDirectoryExist path
                    if exists then createDirectory path
                    else return ()
                    B.writeFile (path </> filename) $ C.pack $ rspBody b
                    privmsg h generalChan ("Successfully saved " ++ filename)
    Help -> print "Help called." >> mapM (privmsg h generalChan) help >> return ()
    List -> do
        print "List called."
        contents <- listDirectory path -- handle errors
        mapM (privmsg h generalChan) contents
        return ()

printCommand :: Command -> [String] -> IO ()
printCommand com args = do
    print $ show com ++ " called with:\n"
    mapM print args
    return ()

toCamel :: String -> String
toCamel [] = []
toCamel (x:xs) = (toUpper x) : map toLower xs

main = do
    h <- connectTo ircServer (PortNumber (fromIntegral ircPort))
    hSetBuffering h NoBuffering
    write h "NICK" name
    threadDelay 1000
    write h "USER" $ name ++ " 0 * :" ++ name
    threadDelay 1000
    write h "JOIN" generalChan
    listen h
