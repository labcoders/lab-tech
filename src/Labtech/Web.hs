{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Labtech.Web
( main
) where

import qualified Labtech.InternalMessaging.Types as IM
import qualified Labtech.InternalMessaging as IM

import Control.Monad.IO.Class
import Control.Concurrent.Chan
import Data.Aeson ( Object )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant
import Servant.GitHub.Webhook

-- | Entry point for the built-in web server.
main :: Chan IM.WorkerRegistration -> IO ()
main _ = do
  putStrLn "Starting built-in webserver."
  key <- BS.init <$> BS.readFile "hook-secret" -- init to remove newline
  putStrLn $ "loaded key " ++ show key
  run 13337 (app (gitHubKey $ pure key)) -- TODO register web server with chan

app :: GitHubKey -> Application
app key
  = serveWithContext
    (Proxy :: Proxy LabtechAPI)
    (key :. EmptyContext)
    labtechServer

labtechServer :: Server LabtechAPI
labtechServer = pushEvent

pushEvent :: RepoWebhookEvent -> ((), Object) -> Handler ()
pushEvent _ _ = do
  liftIO $ do
    putStrLn "got push!"

  pure ()

type LabtechAPI
  = "github"
    :> GitHubEvent '[ 'WebhookPushEvent ]
    :> GitHubSignedReqBody '[JSON] Object
    :> Post '[JSON] ()
