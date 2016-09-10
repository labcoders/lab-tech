{-|
 - Description: Stuff for dealing with Github webhooks
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Labtech.Web.Github
( module Labtech.Web.Github
, module GitHub.Data.Webhooks
) where

import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString as BS
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Base16 as B16
import Data.HMAC ( hmac_sha1 )
import Data.Maybe ( fromMaybe )
import Data.Proxy
import Data.String.Conversions ( cs )
import qualified Data.Text.Encoding as E
import GHC.TypeLits
import GitHub.Data.Webhooks
import Network.HTTP.Types hiding (Header, ResponseHeaders)
import Network.Wai ( requestHeaders, strictRequestBody )
import Servant
import Servant.API.ContentTypes ( AllCTUnrender(..) )
import Servant.Server.Internal

data GithubSignedReqBody (list :: [*]) (result :: *)

newtype GithubKey = GithubKey { unGithubKey :: IO BS.ByteString }

instance forall sublayout context list result.
  ( HasServer sublayout context
  , HasContextEntry context GithubKey
  , AllCTUnrender list result
  )
  => HasServer (GithubSignedReqBody list result :> sublayout) context where

  type ServerT (GithubSignedReqBody list result :> sublayout) m
    = result -> ServerT sublayout m

  route
    :: forall env. Proxy (GithubSignedReqBody list result :> sublayout)
    -> Context context
    -> Delayed env (result -> Server sublayout)
    -> Router env
  route _ context subserver
    = route (Proxy :: Proxy sublayout) context (addBodyCheck subserver go)
    where
      lookupSig = lookup "X-Hub-Signature"

      go :: DelayedIO result
      go = withRequest $ \req -> do
        let hdrs = requestHeaders req
        key <- BS.unpack <$> liftIO (unGithubKey $ getContextEntry context)
        msg <- BS.unpack <$> liftIO (toStrict <$> strictRequestBody req)
        let sig = B16.encode $ BS.pack $ hmac_sha1 key msg
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ hdrs
        let mrqbody =
              handleCTypeH (Proxy :: Proxy list) (cs contentTypeH) $
              fromStrict (BS.pack msg)

        case mrqbody of
          Nothing        -> delayedFailFatal err415
          Just (Left e)  -> delayedFailFatal err400 { errBody = cs e }
          Just (Right v) -> case parseHeaderMaybe =<< lookupSig hdrs of
            Nothing -> do
              liftIO $ putStrLn "no X-Hub-Signature"
              delayedFailFatal err401
            Just h -> do
              let h' = BS.drop 5 $ E.encodeUtf8 h -- remove "sha1=" prefix
              if h' == sig
              then pure v
              else do
                liftIO $ putStrLn $ concat
                  [ "computed signature ", show sig, "doesn't match given "
                  , "signature ", show h'
                  ]
                delayedFailFatal err401

data XGithubEvent (event :: RepoWebhookEvent)

instance forall sublayout context event.
  (Reflect event, HasServer sublayout context)
  => HasServer (XGithubEvent event :> sublayout) context where

  type ServerT (XGithubEvent event :> sublayout) m
    = RepoWebhookEvent -> ServerT sublayout m

  route
    :: forall env. Proxy (XGithubEvent event :> sublayout)
    -> Context context
    -> Delayed env (RepoWebhookEvent -> Server sublayout)
    -> Router env
  route Proxy context subserver
    = route
      (Proxy :: Proxy sublayout)
      context
      (addAuthCheck subserver go)
    where
      lookupGHEvent = lookup "X-Github-Event"

      go :: DelayedIO RepoWebhookEvent
      go = withRequest $ \req -> do
        case parseHeaderMaybe =<< lookupGHEvent (requestHeaders req) of
          Nothing -> do
            liftIO $ putStrLn "header parse failed"
            delayedFail err401
          Just h ->
            let event = reflect (Proxy :: Proxy event)
            in if h == eventName event
               then do
                 pure (reflect (Proxy :: Proxy event))
               else do
                 liftIO $ putStrLn $ concat
                   [ "route for ", show event, " does not match received "
                   , "event type `", h, "`."
                   ]
                 delayedFail err400

type family Demote' (kparam :: KProxy k) :: *
type Demote (a :: k) = Demote'('KProxy :: KProxy k)

type instance Demote' ('KProxy :: KProxy Symbol) = String
type instance Demote' ('KProxy :: KProxy [k]) = [Demote' ('KProxy :: KProxy k)]
type instance Demote' ('KProxy :: KProxy RepoWebhookEvent) = RepoWebhookEvent

class Reflect (a :: k) where
  reflect :: Proxy (a :: k) -> Demote a

eventName :: RepoWebhookEvent -> String
eventName e = case e of
  WebhookWildcardEvent -> "*"
  WebhookCommitCommentEvent -> "commit_comment"
  WebhookCreateEvent -> "create"
  WebhookDeleteEvent -> "delete"
  WebhookDeploymentEvent -> "deployment"
  WebhookPushEvent -> "push"

instance KnownSymbol s => Reflect (s :: Symbol) where
  reflect = symbolVal

instance Reflect 'WebhookWildcardEvent where
  reflect _ = WebhookWildcardEvent

instance Reflect 'WebhookCommitCommentEvent where
  reflect _ = WebhookCommitCommentEvent

instance Reflect 'WebhookCreateEvent where
  reflect _ = WebhookCreateEvent

instance Reflect 'WebhookDeleteEvent where
  reflect _ = WebhookDeleteEvent

instance Reflect 'WebhookDeploymentEvent where
  reflect _ = WebhookDeploymentEvent

instance Reflect 'WebhookDeploymentStatusEvent where
  reflect _ = WebhookDeploymentStatusEvent

instance Reflect 'WebhookForkEvent where
  reflect _ = WebhookForkEvent

instance Reflect 'WebhookGollumEvent where
  reflect _ = WebhookGollumEvent

instance Reflect 'WebhookIssueCommentEvent where
  reflect _ = WebhookIssueCommentEvent

instance Reflect 'WebhookIssuesEvent where
  reflect _ = WebhookIssuesEvent

instance Reflect 'WebhookMemberEvent where
  reflect _ = WebhookMemberEvent

instance Reflect 'WebhookPageBuildEvent where
  reflect _ = WebhookPageBuildEvent

instance Reflect 'WebhookPublicEvent where
  reflect _ = WebhookPublicEvent

instance Reflect 'WebhookPullRequestReviewCommentEvent where
  reflect _ = WebhookPullRequestReviewCommentEvent

instance Reflect 'WebhookPullRequestEvent where
  reflect _ = WebhookPullRequestEvent

instance Reflect 'WebhookPushEvent where
  reflect _ = WebhookPushEvent

instance Reflect 'WebhookReleaseEvent where
  reflect _ = WebhookReleaseEvent

instance Reflect 'WebhookStatusEvent where
  reflect _ = WebhookStatusEvent

instance Reflect 'WebhookTeamAddEvent where
  reflect _ = WebhookTeamAddEvent

instance Reflect 'WebhookWatchEvent where
  reflect _ = WebhookWatchEvent

parseHeaderMaybe :: FromHttpApiData a => BS.ByteString -> Maybe a
parseHeaderMaybe = eitherMaybe . parseHeader where
  eitherMaybe :: Either e a -> Maybe a
  eitherMaybe e = case e of
    Left _ -> Nothing
    Right x -> Just x
