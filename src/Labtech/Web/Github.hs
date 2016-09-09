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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Labtech.Web.Github
( module Labtech.Web.Github
, module GitHub.Data.Webhooks
) where

import Control.Monad.IO.Class ( liftIO )
import qualified Data.ByteString as BS
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GitHub.Data.Webhooks
import Network.Wai ( requestHeaders )
import Servant
import Servant.Server.Internal

data XGithubSignature (key :: Symbol)

instance forall sublayout context key.
  (HasServer sublayout context, KnownSymbol key)
  => HasServer (XGithubSignature key :> sublayout) context where

  type ServerT (XGithubSignature key :> sublayout) m
    = () -> ServerT sublayout m

  route
    :: forall env. Proxy (XGithubSignature key :> sublayout)
    -> Context context
    -> Delayed env (() -> Server sublayout)
    -> Router env
  route = undefined

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

      parseHeaderMaybe :: FromHttpApiData a => BS.ByteString -> Maybe a
      parseHeaderMaybe = eitherMaybe . parseHeader where
        eitherMaybe :: Either e a -> Maybe a
        eitherMaybe e = case e of
          Left _ -> Nothing
          Right x -> Just x

type family Demote' (kparam :: KProxy k) :: Type
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
