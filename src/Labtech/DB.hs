{-# LANGUAGE OverloadedStrings #-}

module Labtech.DB where

import Control.Exception

import Data.Int
import Data.UID

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

import qualified Data.Text as T

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
uploadSelectStr = "select url, title, filepath, uploadtime, nick from uploads"

uploadInsertStr :: Query
uploadInsertStr = "insert into uploads (url, title, filepath, uploadtime, nick) values (?, ?, ?, DEFAULT, ?)"

uploadContainsStr :: Query
uploadContainsStr = "select url, title, filepath, uploadtime, nick from uploads where ? = ?"

ideaContainsStr :: Query
ideaContainsStr = "select idea from ideas where idea = ?"

ideaInsertStr :: Query
ideaInsertStr = "insert into ideas (idea) values (?)"

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

insertIdea :: String -> Nick-> IO String
insertIdea id nick = do
    conn <- connect labtechConnInfo
    r <- (try $ execute conn ideaInsertStr (Only id)) :: IO (Either SqlError Int64)
    case r of
            Left ex -> return $ "Failed to upload idea\"" ++ 
                                id ++ " from " ++ (unNick nick) ++ 
                                ". Exception was: " ++ 
                                (displayException ex)
            Right i -> return $ "Successfully uploaded idea\"" ++ 
                                id ++ " from " ++ (unNick nick)

ideaTableContains :: String -> IO Bool
ideaTableContains s = do
    conn <- connect labtechConnInfo
    is <- query conn ideaContainsStr (Only s) :: IO [UploadEntry]
    return $ length is /= 0

uploadTableContains :: String -> IO Bool
uploadTableContains s = do
    conn <- connect labtechConnInfo
    urls <- query conn uploadContainsStr (toField ("url" :: String), toField s) :: IO [UploadEntry]
    tits <- query conn uploadContainsStr (toField ("title" :: String), toField s) :: IO [UploadEntry]
    return (length urls /= 0 || length tits /= 0)

getUniqueName :: IO String
getUniqueName = do
    cts <- listDirectory uploadFilePath
    uid <- newUIDString
    if elem uid cts then getUniqueName
    else return uid

queryUploads :: IO [UploadEntry]
queryUploads = do
    conn <- connect labtechConnInfo
    print =<< formatQuery conn uploadSelectStr ()
    query_ conn uploadSelectStr

insertUpload :: Url -> Title -> FilePath -> Nick -> IO String
insertUpload url tit fp nick = do
    conn <- connect labtechConnInfo
    print =<< formatQuery conn uploadInsertStr ((C.pack url :: C.ByteString), tit :: String, (C.pack fp :: C.ByteString), (unNick nick) :: String)
    r <- (try $ execute conn uploadInsertStr ((C.pack url :: C.ByteString), tit :: String, (C.pack fp :: C.ByteString), (unNick nick) :: String)) :: IO (Either SqlError Int64)
    case r of
        Left ex -> return $ "Failed to upload \"" ++
                            tit ++ "\" (" ++ url ++ ") from " ++
                            (unNick nick) ++ ". Exception was: " ++
                            (displayException ex)
        Right i -> return $ "Successfully uploaded \"" ++
                            tit ++ "\" (" ++ url ++ ") from " ++
                            (unNick nick)
