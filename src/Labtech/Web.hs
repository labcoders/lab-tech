{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Labtech.Web
( main
) where

import qualified Labtech.InternalMessaging.Types as IM
import qualified Labtech.InternalMessaging as IM
import Labtech.Web.Github

import Control.Monad.IO.Class
import Control.Concurrent.Chan
import Data.Aeson ( Object )
import qualified Data.Text as T
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant


-- | Entry point for the built-in web server.
main :: Chan IM.WorkerRegistration -> IO ()
main _ = do
  putStrLn "Starting built-in webserver."
  run 13337 app -- TODO register web server with chan

app :: Application
app = serve (Proxy :: Proxy LabtechAPI) labtechServer

labtechServer :: Server LabtechAPI
labtechServer = pushEvent

pushEvent :: RepoWebhookEvent -> Object -> Handler ()
pushEvent _ obj = do
  liftIO $ do
    putStrLn "got push!"

  pure ()

type LabtechAPI
  = "github"
    :> XGithubEvent 'WebhookPushEvent
    :> ReqBody '[JSON] Object
    :> Post '[JSON] ()
