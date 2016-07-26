{-# LANGUAGE ViewPatterns #-}

module Labtech where

import Labtech.Command
import Labtech.Command.Types
import qualified Labtech.InternalMessaging.Types as IM
import Labtech.IRC
import Labtech.IRC.Types
import Labtech.Types

import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.List ( isPrefixOf )
import qualified Data.Map as M
import Network
import System.IO

-- | Runs an infinite loop that creates a labtech bot for the given server
-- specification.
runOnServer :: Chan IM.WorkerRegistration -> ServerSpec -> IO ()
runOnServer chan spec = do
  h <- connectTo (serverHost spec) (PortNumber (fromIntegral $ serverPort spec))
  hSetBuffering h NoBuffering
  ircEnv <- makeIrcEnv h spec

  mmsgChan <- newEmptyMVar

  void $ forkIO $ runLabtech ircEnv simpleLabEnv $ do
    recvChan <- liftIO newChan
    liftIO $ writeChan chan $ IM.WorkerRegistration
      { IM.regName = serverWorkerName spec
      , IM.regChan = recvChan
      }
    IM.InternalMessage { IM.intmsgBody = resp } <- liftIO $ readChan recvChan

    serverChan <- case resp of
      IM.RegistrationResult res -> case res of
        IM.RegistrationOk serverChan -> pure serverChan
        IM.RegistrationFailed reason -> liftIO $ print reason *> error "dead"
      _ -> liftIO $ putStrLn "unexpected message" *> error "dead"

    liftIO $ putMVar mmsgChan serverChan

    forever $ do
      IM.InternalMessage { IM.intmsgBody = body } <- liftIO $ readChan recvChan
      case body of
        IM.Privmsg target contents -> ircPrivmsg target contents
        x -> liftIO $ putStrLn $ "unhandled internal message: " ++ show x

  msgChan <- takeMVar mmsgChan

  runLabtech ircEnv simpleLabEnv $ do
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

type CommandResponse = [String]

mainLoop :: Chan IM.InternalMessageWS -> ServerSpec -> Labtech a
mainLoop chan spec = forever $ handleMessage chan spec =<< ircNext

handleMessage :: Chan IM.InternalMessageWS -> ServerSpec -> Message -> Labtech ()
handleMessage chan spec message = case message of
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
