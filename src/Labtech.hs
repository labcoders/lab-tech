{-# LANGUAGE ViewPatterns #-}

module Labtech where

import Labtech.Async
import Labtech.Command
import Labtech.Command.Types
import qualified Labtech.InternalMessaging.Types as IM
import Labtech.IRC
import Labtech.IRC.Types
import Labtech.Types

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Reader
import Data.Default.Class
import Data.List ( isPrefixOf )
import qualified Data.Map as M
import Network
import Network.Connection hiding ( connectTo )
import System.IO

-- | Runs an infinite loop that creates a labtech bot for the given server
-- specification.
runOnServer :: Chan IM.WorkerRegistration -> ServerSpec -> IO ()
runOnServer chan spec = void $ restarting (unregister chan spec) $ do
  h <- connectTo (serverHost spec) (PortNumber (fromIntegral $ serverPort spec))
  hSetBuffering h NoBuffering
  ircEnv <- if useSSL spec then do
              ctx <- initConnectionContext
              conn <- connectFromHandle ctx h $ toConnParams spec
              makeIrcEnv conn spec
            else makeIrcEnv h spec

  mmsgChan <- newEmptyMVar

  serverMsgT <- async $ runLabtech spec ircEnv simpleLabEnv $ do
    recvChan <- liftIO newChan
    liftIO $ do
      writeChan chan $ IM.WorkerRegistration (serverWorkerName spec) recvChan
    IM.InternalMessage { IM.intmsgBody = resp } <- liftIO $ readChan recvChan

    case resp of
      IM.RegistrationResult (IM.RegistrationOk serverChan) -> do
        liftIO $ putMVar mmsgChan serverChan
        forever $ do
          IM.InternalMessage { IM.intmsgBody = body } <- liftIO $ readChan recvChan
          case body of
            IM.Privmsg target contents -> ircPrivmsg target contents
            x -> liftIO $ putStrLn $ "unhandled internal message: " ++ show x
      _ -> do
        liftIO $ putStrLn "unexpected message / registration failed"
        error "TODO: throw exception"

  msgChan <- takeMVar mmsgChan

  ircBotT <- async $ runLabtech spec ircEnv simpleLabEnv $ do
    -- do the IRC login and begin the main loop
    liftIO $ threadDelay 3000000
    ircUser (serverUsername spec) (serverRealName spec)
    liftIO $ threadDelay 3000000
    ircNick (head $ serverNicks spec)
    handleMessage msgChan spec =<< ircNext
    liftIO $ threadDelay 3000000
    forM_ (serverChannels spec) $ \c -> do
      ircJoin c
      liftIO $ threadDelay 1000000
    mainLoop msgChan spec

  waitBoth serverMsgT ircBotT

toConnParams :: ServerSpec -> ConnectionParams
toConnParams spec
  = ConnectionParams
  { connectionHostname = serverHost spec
  , connectionPort = fromIntegral $ serverPort spec
  , connectionUseSecure = if useSSL spec then Just def else Nothing
  , connectionUseSocks = Nothing -- no proxies.
  }

type CommandResponse = [String]

mainLoop :: Chan IM.InternalMessageWS -> ServerSpec -> Labtech a
mainLoop chan spec = forever $ handleMessage chan spec =<< ircNext

handleMessage
  :: MonadLabIrcIO m
  => Chan IM.InternalMessageWS
  -> ServerSpec
  -> Message
  -> m ()
handleMessage chan spec message = case message of
  NickInUse -> labRenick
  Pingmsg ping -> ircPong ping
  Privmsg origin target (init -> body) -> do
    liftIO $ putStrLn $ concat
      [ unNick . originNick $ origin, " (", renderTarget target, ") :", body ]

    when ("!" `isPrefixOf` body) $ do
      let env = CommandEnv
            { commandSender = originNick origin
            , commandBody = body
            , commandServerSpec = spec
              , commandTarget = target
              }
      case parseCommand env of
        Left err -> mapM_ (ircPrivmsg target) (lines err)
        Right command -> handleCommand env { commandBody = command }

    liftIO $ replicateMessage spec origin target chan body

replicateMessage
  :: ServerSpec
  -> MessageOrigin
  -> MessageTarget
  -> Chan IM.InternalMessageWS
  -> String
  -> IO ()
replicateMessage _ _ (NickTarget _) _ _ = pure ()
replicateMessage spec origin (ChannelTarget channel) chan msg = do
  case M.lookup channel (serverReplication spec) of
    Nothing -> putStrLn "received message on non-replicated channel"
    Just targets -> forM_ targets $ \target -> do
      writeChan chan $ IM.InternalMessage
        { IM.intmsgSender = serverWorkerName spec
        , IM.intmsgBody
          = IM.InterServerPrivmsg
            (replicationServer target)
            (replicationTarget target)
            (ChannelTarget channel)
            origin
            msg
        }

-- | Unregisters the worker for the given server spec from the internal
-- messaging system.
unregister
  :: Chan IM.WorkerRegistration
  -> ServerSpec
  -> SomeException
  -> IO (Maybe ())
unregister chan spec _ = do
  writeChan chan $ IM.WorkerUnregistration (serverWorkerName spec)
  pure Nothing
