{-|
 - Description: Filesystem interactions.
 -}

module Labtech.FS
( saveLink
) where

import Labtech.Types

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.UID
import Network.HTTP
import System.Directory ( listDirectory )
import System.FilePath

uploadFilePath :: FilePath
uploadFilePath = "data"

-- We have really bad UIDs because i'm quite tired.
saveLink :: Url -> Title -> IO (Maybe FilePath)
saveLink url _ = do
    result <- simpleHTTP $ getRequest url
    fn <- getUniqueName
    let fp = uploadFilePath </> fn
    case result of
        Left _ -> pure Nothing
        Right rp -> do
            B.writeFile fp $ T.encodeUtf8 $ T.pack $ rspBody rp
            pure $ Just fp

getUniqueName :: IO String
getUniqueName = do
    cts <- listDirectory uploadFilePath
    uid <- newUIDString
    if elem uid cts then getUniqueName else pure uid
