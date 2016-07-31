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
    reg  <- readChan workerChan
    modifyMVar_ mmap $ \m -> do
      case reg of
        WorkerRegistration name chan -> do
          let name' = unWorkerName name
          case M.lookup name m of
            Just _ -> do
              putStrLn $ "IM: registration failed for " ++ name' ++ ": name in use."
              writeChan chan $ InternalMessage
                { intmsgSender = ()
                , intmsgBody = RegistrationResult (RegistrationFailed WorkerNameInUse)
                }
              pure m
            Nothing -> do
              putStrLn $ "IM: registration successful for " ++ name'
              writeChan chan $ InternalMessage
                { intmsgSender = ()
                , intmsgBody = RegistrationResult (RegistrationOk msgChan)
                }
              pure $ M.insert name chan m
        WorkerUnregistration name -> do
          let name' = unWorkerName name
          case M.lookup name m of
            Just _ -> do
              putStrLn $ "IM: unregistering worker: " ++ name'
              pure (M.delete name m)
            Nothing -> do
              putStrLn $ "IM: received unregister request for " ++ name' ++
                " but " ++ name' ++ " is not registered."
              pure m

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
          putStrLn $ "IM: received interserver privmsg from " ++ unWorkerName sender
          let nick = originNick origin
          case M.lookup name m of
            Nothing ->
              putStrLn $ "worker not sign in for replication: " ++ unWorkerName name
            Just dchan -> do
              writeChan dchan $ InternalMessage
                { intmsgSender = ()
                , intmsgBody = Privmsg recp ("[" ++ unNick nick ++ "] " ++ body')
                }
        x -> putStrLn $ "Unhandled internal message " ++ show x
