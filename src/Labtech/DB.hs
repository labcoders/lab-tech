{-# LANGUAGE OverloadedStrings #-}

module Labtech.DB where

import Control.Exception

import Data.Int
import Data.UID

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import Labtech.Types
import Labtech.DB.Types
import Labtech.IRC.Types

import Network.HTTP

import System.Directory
import System.FilePath.Posix

uploadFilePath :: FilePath
uploadFilePath = "/home/labtech/data"

labtechConnInfo :: ConnectInfo
labtechConnInfo = defaultConnectInfo
                { connectUser = "labtech"
                , connectDatabase = "labtech"
                }

formatEntry :: UploadEntry -> String
formatEntry (UploadEntry 
               { uploadUrl = url
               , uploadTitle = title
               , uploadNick = nick
               })
               = title ++ " (" ++ url ++ ") - uploaded by " ++ (unNick nick)

uploadSelectStr :: Query
uploadSelectStr = "select " ++
                  "(url, title, filepath, uploadtime, nick) " ++
                  "from uploads"

uploadInsertStr :: Query
uploadInsertStr = "insert into " ++
            "(url, title, filepath, nick) values ?, ?, ?, ?"

containsStr :: Query
containsStr = "select " ++
            "(url, title, filepath, uploadtime, nick) " ++
            "from uploads where ? = ?"

-- We have really bad UIDs because i'm quite tired.
saveLink :: Url -> Title -> IO (Maybe FilePath)
saveLink url tit = do
    result <- simpleHTTP $ getRequest url
    case result of
        Left err -> return Nothing
        Right rp -> do
            fn <- getUniqueName
            B.writeFile (uploadFilePath </> fn) $ C.pack $ rspBody rp
            return $ Just (uploadFilePath </> fn)

dbContains :: String -> IO Bool
dbContains s = do
    conn <- connect labtechConnInfo
    urls <- query conn containsStr (toField ("url" :: String), toField s) :: IO [UploadEntry]
    tits <- query conn containsStr (toField ("title" :: String), toField s) :: IO [UploadEntry]
    return (length urls /= 0 || length tits /= 0)

getUniqueName :: IO String
getUniqueName = do
    cts <- listDirectory uploadFilePath
    uid <- newUIDString
    if elem uid cts then getUniqueName
    else return uid

queryUploads :: IO [UploadEntry]
queryUploads = flip query_ uploadSelectStr =<< connect labtechConnInfo -- catch errors

insertUpload :: Url -> Title -> FilePath -> Nick -> IO String
insertUpload url tit fp nick = do
    conn <- connect labtechConnInfo
    r <- (try $ execute conn uploadInsertStr (url, tit, fp, unNick nick)) :: IO (Either SqlError Int64)
    case r of
        Left ex -> return $ "Failed to upload \"" ++ 
                            tit ++ "\" (" ++ url ++ ") from " ++ 
                            (unNick nick) ++ ". Exception was: " ++ 
                            (displayException ex)
        Right i -> return $ "Successfully uploaded \"" ++ 
                            tit ++ "\" (" ++ url ++ ") from " ++ 
                            (unNick nick)
