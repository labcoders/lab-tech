{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader

import Data.Bifunctor ( first )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.Text as T

import Network
import Network.HTTP

import System.FilePath.Posix
import System.IO
import System.Directory

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Read
import Text.Printf

data Command
    = Upload Url FilePath
    | Help
    | List
    deriving (Show, Read)

data ServerSpec
  = ServerSpec
    { serverNicks :: [Nick]
    , serverHost :: String
    , serverPort :: Int
    , serverChannels :: [Channel]
    , serverUsername :: Username
    , serverRealName :: RealName
    }

labcodersSpec :: ServerSpec
labcodersSpec = ServerSpec
  { serverNicks = Nick <$> [ "labtech", "labtech_", "labtech__" ]
  , serverUsername = Username "labtech"
  , serverRealName = RealName "labtech"
  , serverHost = "labcoders.club"
  , serverPort = 6667
  , serverChannels = Channel <$> [ "#general" ]
  }

freenodeSpec :: ServerSpec
freenodeSpec = ServerSpec
  { serverNicks = Nick <$> [ "labtech", "labtech_", "labtech__" ]
  , serverHost = "irc.freenode.net"
  , serverUsername = Username "labtech"
  , serverRealName = RealName "labtech"
  , serverPort = 6667
  , serverChannels = Channel <$> [ "#labcoders" ]
  }

path :: FilePath
path = "data"

help =
     [ "Available commands:"
     , "!help -> Print help"
     , "!upload [url] [filename] -> Downloads the provided url to the data directory with the provided filename."
     , "!list -> List all available files downloaded in the data directory."
     ]

write :: Handle -> String -> String -> IO ()
write h a b = do
    hPrintf h "%s %s\r\n" a b
    printf "> %s %s\n" a b

newtype Nick = Nick { unNick :: String }
newtype Username = Username { unUsername :: String }
newtype RealName = RealName { unRealName :: String }
newtype Channel = Channel { unChannel :: String }
newtype Ping = Ping { unPing :: String }

ping :: String -> Maybe Ping
ping s = if "PING :" `isPrefixOf` s then Just (Ping $ drop 6 s) else Nothing

-- | Concrete 'IO'-based interpreter for IRC commands.
data IrcEnv
  = IrcEnv
    { _nickE :: Nick -> IO ()
    , _userE :: Username -> RealName -> IO ()
    , _joinE :: Channel -> IO ()
    , _pongE :: Ping -> IO ()
    , _nextE :: IO Message
    , _privmsgE :: MessageTarget -> String -> IO ()
    }

makeIrcEnv :: Handle -> ServerSpec -> IrcEnv
makeIrcEnv h spec = IrcEnv
  { _nickE = write h "NICK" . unNick
  , _joinE = write h "JOIN" . unChannel
  , _pongE = write h "PONG" . (':' :) . unPing
  , _nextE = _next
  , _userE = _user
  , _privmsgE = _msg
  } where
    _user (Username username) (RealName realname)
      = write h "USER" $ username ++ " 0 * :" ++ realname

    _next = do
      line <- hGetLine h
      liftIO $ putStrLn $ "< " ++ line
      case parseMessage line of
        Left err -> do
          -- putStrLn $ "parse error: " ++ err
          _next
        Right x -> pure x

    _msg target msg = do
      write h "PRIVMSG" $ renderTarget target ++ " :" ++ msg

data MessageTarget
  = ChannelTarget Channel
  | NickTarget Nick

targetToEither :: MessageTarget -> Either Channel Nick
targetToEither m = case m of
  ChannelTarget x -> Left x
  NickTarget x -> Right x

renderTarget :: MessageTarget -> String
renderTarget = either unChannel unNick . targetToEither

data MessageOrigin
  = MessageOrigin
    { originNick :: Nick
    , originHost :: String
    }

data Message
  = Privmsg MessageOrigin MessageTarget String
  | Pingmsg Ping

parseMessage :: String -> Either String Message
parseMessage = first parseErrorPretty . runParser messageParser "irc" where
  messageParser :: Parser Message
  messageParser = privmsg <|> ping

  privmsg :: Parser Message
  privmsg = do
    origin <- try $ msgorigin <* many spaceChar <* string' "privmsg"
    skipMany spaceChar
    target <- msgtarget
    many spaceChar
    string ":"
    msg <- anyChar `manyTill` eof
    pure $ Privmsg origin target msg

  ping :: Parser Message
  ping = do
    try $ string' "ping"
    skipMany spaceChar
    string ":"
    msg <- anyChar `manyTill` eof
    pure $ Pingmsg (Ping msg)

  msgtarget :: Parser MessageTarget
  msgtarget = (ChannelTarget <$> channel) <|> (NickTarget <$> nick) where
    channel
      = fmap Channel $ (++) <$> try (string "#") <*> (anyChar `someTill` spaceChar)
    nick
      = fmap Nick $ (anyChar `someTill` spaceChar)

  msgorigin :: Parser MessageOrigin
  msgorigin = do
    string ":"
    nick <- anyChar `someTill` string "!"
    host <- anyChar `someTill` spaceChar
    pure $ MessageOrigin (Nick nick) host

data CommandEnv command
  = CommandEnv
    { commandSender :: Nick
    , commandBody :: command
    , commandChannel :: MessageTarget
    }
  deriving (Functor)

parseCommand :: ServerSpec -> String -> Either String Command
parseCommand spec = first parseErrorPretty . runParser commandParser name where
  name = "irc:" ++ server

class MonadIRC m where
  ircNick :: Nick -> m ()
  ircUser :: Username -> RealName -> m ()
  ircJoin :: Channel -> m ()
  ircPong :: Ping -> m ()
  ircNext :: m Message
  ircPrivmsg :: MessageTarget -> String -> m ()

instance MonadIO m => MonadIRC (ReaderT IrcEnv m) where
  ircNick nick = do
    f <- asks _nickE
    liftIO $ f nick
  ircUser user real = do
    f <- asks _userE
    liftIO $ f user real
  ircJoin chan = do
    f <- asks _joinE
    liftIO $ f chan
  ircPong ping = do
    f <- asks _pongE
    liftIO $ f ping
  ircNext = liftIO =<< asks _nextE
  ircPrivmsg target msg = do
    f <- asks _privmsgE
    liftIO $ f target msg

-- | A constraint in which the monad @t n@ can perform IRC actions as well as
-- IO actions.
type MonadIrcIO t n = (MonadTrans t, Monad n, Monad (t n), MonadIO n, MonadIRC (t n))

type FileListing = [String]
type Url = String

-- | Concrete 'IO'-based interpreter for Labtech commands.
data LabEnv
  = LabEnv
    { _listE
        :: forall t n. MonadIrcIO t n => MessageTarget -> t n FileListing
    , _uploadE
        :: forall t n. MonadIrcIO t n => Url -> FilePath -> MessageTarget -> t n ()
    , _helpE
        :: forall t n. MonadIrcIO t n => MessageTarget -> t n ()
    }

class MonadLab m where
  labList :: MessageTarget -> m ()
  labUpload :: Url -> FilePath -> MessageTarget -> m ()
  labHelp :: MessageTarget -> m ()

makeLabEnv :: LabEnv
makeLabEnv = LabEnv
  { _helpE = \target -> forM_ help $ \line -> do
      ircPrivmsg target line
  , _listE = error "List unsupported."
  , _uploadE = error "Upload unsupported."
  }

instance MonadIO m => MonadIRC (ReaderT (IrcEnv, LabEnv) m) where
  ircNick = withReaderT fst . ircNick
  ircUser = withReaderT fst .% ircUser where (.%) = (.) . (.)
  ircJoin = withReaderT fst . ircJoin
  ircPong = withReaderT fst . ircPong
  ircNext = withReaderT fst ircNext
  ircPrivmsg = withReaderT fst .% ircPrivmsg where (.%) = (.) . (.)

instance MonadIO m => MonadLab (ReaderT (IrcEnv, LabEnv) m) where
  labHelp target = do
    f <- asks (_helpE . snd)
    f target
  labUpload = error "unimplemented: upload"
  labList = error "unimplemented: list"

type Labtech = ReaderT (IrcEnv, LabEnv) IO

runLabtech :: IrcEnv -> LabEnv -> Labtech a -> IO a
runLabtech irc lab m = runReaderT m (irc, lab)

main = do
  mapM_ (forkIO . runOnServer) [ labcodersSpec, freenodeSpec ]
  putStrLn "press return to quit"
  getLine

runOnServer :: ServerSpec -> IO ()
runOnServer spec = do
  h <- connectTo (serverHost spec) (PortNumber (fromIntegral $ serverPort spec))
  hSetBuffering h NoBuffering
  runLabtech (makeIrcEnv h) makeLabEnv $ do
      liftIO $ threadDelay 3000000
      ircUser (serverUsername spec) (serverRealName spec)
      liftIO $ threadDelay 3000000
      ircNick (head $ serverNicks spec)
      handleMessage =<< ircNext
      liftIO $ threadDelay 3000000
      forM_ (serverChannels spec) $ \chan -> do
        ircJoin chan
        liftIO $ threadDelay 1000000
      mainLoop

type CommandResponse = [String]

mainLoop :: Labtech a
mainLoop = forever $ handleMessage =<< ircNext

handleMessage :: Message -> Labtech ()
handleMessage message = case message of
  Privmsg origin target body -> do
    liftIO $ putStrLn $ concat
      [ unNick . originNick $ origin, " (", renderTarget target, ") :", body ]
  Pingmsg ping -> do
    ircPong ping
