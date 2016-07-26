{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Labtech.InternalMessaging where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad ( forever, void )
import qualified Data.Map as M

import Labtech.InternalMessaging.Types
import Labtech.IRC.Types hiding ( Privmsg )
import Labtech.Orphans ()

mainLoop :: Chan WorkerRegistration -> IO a
mainLoop workerChan = do
  -- map to associate worker names with their bichans
  mmap <- newMVar M.empty
  msgChan <- newChan :: IO (Chan InternalMessageWS) -- channel to receive internal messages

  void $ forkIO $ forever $ processMessage mmap =<< readChan msgChan

  forever $ do
    WorkerRegistration { regName = name, regChan = chan }  <- readChan workerChan
    modifyMVar_ mmap $ \m -> do
      case M.lookup name m of
        Just _ -> do
          writeChan chan $ InternalMessage
            { intmsgSender = ()
            , intmsgBody = RegistrationResult (RegistrationFailed WorkerNameInUse)
            }
          pure m
        Nothing -> do
          writeChan chan $ InternalMessage
            { intmsgSender = ()
            , intmsgBody = RegistrationResult (RegistrationOk msgChan)
            }
          pure $ M.insert name chan m

processMessage
  :: MVar (M.Map WorkerName (Chan InternalMessageSW))
  -> InternalMessageWS
  -> IO ()
processMessage mmap (InternalMessage { intmsgSender = sender, intmsgBody = body }) = do
  m <- readMVar mmap
  let mchan = M.lookup sender m
  case mchan of
    Nothing -> do
      putStrLn $ "received message from unregistered worker: " ++ unWorkerName sender
    Just _ -> do
      case body of
        InterServerPrivmsg name recp _ origin body' -> do
          let nick = originNick origin
          case M.lookup name m of
            Nothing ->
              putStrLn $ "worker not signed in for replication: " ++ unWorkerName name
            Just dchan -> do
              writeChan dchan $ InternalMessage
                { intmsgSender = ()
                , intmsgBody = Privmsg recp ("[" ++ unNick nick ++ "] " ++ body')
                }
        x -> putStrLn $ "Unhandled internal message " ++ show x
