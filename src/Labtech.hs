{-# LANGUAGE ViewPatterns #-}

module Labtech where

import Labtech.Command
import Labtech.Command.Types
import Labtech.IRC
import Labtech.IRC.Types
import Labtech.Types

import Control.Concurrent ( threadDelay )
import Control.Monad.Reader
import Data.List ( isPrefixOf )
import Network
import System.IO

-- | Runs an infinite loop that creates a labtech bot for the given server
-- specification.
runOnServer :: ServerSpec -> IO ()
runOnServer spec = do
  h <- connectTo (serverHost spec) (PortNumber (fromIntegral $ serverPort spec))
  hSetBuffering h NoBuffering
  ircEnv <- makeIrcEnv h spec
  runLabtech ircEnv simpleLabEnv $ do
      -- do the IRC login and begin the main loop
      liftIO $ threadDelay 3000000
      ircUser (serverUsername spec) (serverRealName spec)
      liftIO $ threadDelay 3000000
      ircNick (head $ serverNicks spec)
      handleMessage spec =<< ircNext
      liftIO $ threadDelay 3000000
      forM_ (serverChannels spec) $ \chan -> do
        ircJoin chan
        liftIO $ threadDelay 1000000
      mainLoop spec

type CommandResponse = [String]

mainLoop :: ServerSpec -> Labtech a
mainLoop spec = forever $ handleMessage spec =<< ircNext

handleMessage :: ServerSpec -> Message -> Labtech ()
handleMessage spec message = case message of
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
