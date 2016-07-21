{-# LANGUAGE ConstraintKinds #-}
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
    = Upload
    | Help
    | List
    deriving (Show, Read)

name :: Nick
name = Nick "labtech"

ircServer :: String
ircServer = "labcoders.club"

ircPort :: Int
ircPort = 6667

generalChan :: Channel
generalChan = Channel "#general"

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

privmsg :: Handle -> Channel -> String -> IO ()
privmsg h (Channel c) s = hPrintf h "PRIVMSG %s %s\r\n" c s

clean :: String -> String
clean = drop 1 . dropWhile (/= ':')

handle :: Handle -> String -> IO ()
handle h [] = return ()
handle h s =
    let cleaned = clean s in
        case cleaned of
            [] -> return ()
            _ -> if head cleaned == '!' then do
                    print $ "Command: " ++ cleaned
                    let args = words $ tail cleaned in do
                        if length args == 0 then return ()
                        else case readMaybe $ toCamel (head args) of
                            Just a -> run h a $ tail args
                            Nothing -> print $ "Failed to parse command: " ++ head args
                 else print cleaned

run :: Handle -> Command -> [String] -> IO ()
run h com args = case com of
    Upload -> do
        print "Upload called."
        if length args /= 2 then privmsg h generalChan $ "Incorrect number of arguments to upload."
        else do
            printCommand com args
            let url = args !! 0
                filename = args !! 1
            print $ "Downloading '" ++ url ++ "' and saving to data/" ++ filename
            response <- simpleHTTP $ getRequest url
            case response of
                Left e -> privmsg h generalChan $ "Failed to save " ++ filename ++ " to disk."
                Right b -> do
                    print $ "Response code: " ++ (show $ rspCode b)
                    print $ "Response reason: " ++ (show $ rspReason b)
                    print "Response headers: "
                    mapM (print . show) $ rspHeaders b
                    exists <- doesDirectoryExist path
                    if exists then createDirectory path
                    else return ()
                    B.writeFile (path </> filename) $ C.pack $ rspBody b
                    privmsg h generalChan ("Successfully saved " ++ filename)
    Help -> print "Help called." >> mapM (privmsg h generalChan) help >> return ()
    List -> do
        print "List called."
        contents <- listDirectory path -- handle errors
        mapM (privmsg h generalChan) contents
        return ()

printCommand :: Command -> [String] -> IO ()
printCommand com args = do
    print $ show com ++ " called with:\n"
    mapM print args
    return ()

toCamel :: String -> String
toCamel [] = []
toCamel (x:xs) = (toUpper x) : map toLower xs

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
    , _privmsgE :: Either Channel Nick -> String -> IO ()
    }

makeIrcEnv :: Handle -> IrcEnv
makeIrcEnv h = IrcEnv
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
      case parseMessage line of
        Left err -> do
          putStrLn $ "parse error: " ++ err
          _next
        Right x -> pure x

    _msg target msg = do
      write h "PRIVMSG" $ either unChannel unNick target ++ " :" ++ msg

type MessageTarget = Either Channel Nick

data Message
  = Privmsg MessageTarget String
  | Pingmsg String

parseMessage :: String -> Either String Message
parseMessage = first show . runParser messageParser "irc" where
  messageParser :: Parser Message
  messageParser = privmsg <|> ping

  privmsg :: Parser Message
  privmsg = do
    try $ string' "privmsg"
    skipMany spaceChar
    target <- msgtarget
    string " :"
    msg <- anyChar `manyTill` eof
    pure $ Privmsg target msg

  ping :: Parser Message
  ping = do
    try $ string' "ping"
    skipMany spaceChar
    msg <- anyChar `manyTill` eof
    pure $ Pingmsg msg

  msgtarget :: Parser (Either Channel Nick)
  msgtarget = (Left <$> channel) <|> (Right <$> nick) where
    channel
      = fmap Channel $ (++) <$> try (string "#") <*> (anyChar `someTill` spaceChar)
    nick
      = fmap Nick $ (anyChar `someTill` spaceChar)

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
    { _listE :: forall t n. MonadIrcIO t n => t n FileListing
    , _uploadE :: forall t n. MonadIrcIO t n => Url -> FilePath -> t n ()
    , _helpE :: forall t n. MonadIrcIO t n => t n ()
    }

class MonadLab m where
  labList :: m ()
  labUpload :: Url -> FilePath -> m ()
  labHelp :: m ()

makeLabEnv :: LabEnv
makeLabEnv = LabEnv
  { _helpE = forM_ help $ \line -> do
      ircPrivmsg (Left generalChan) line
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
  labHelp = asks (_helpE . snd) >>= withReaderT fst
  labUpload = error "unimplemented: upload"
  labList = error "unimplemented: list"

type Labtech = ReaderT (IrcEnv, LabEnv) IO

runLabtech :: IrcEnv -> LabEnv -> Labtech a -> IO a
runLabtech irc lab m = runReaderT m (irc, lab)

main = do
    h <- connectTo ircServer (PortNumber (fromIntegral ircPort))
    hSetBuffering h NoBuffering
    runLabtech (makeIrcEnv h) makeLabEnv $ do
      ircNick name
      liftIO $ threadDelay 1000
      ircUser (Username $ unNick name) (RealName $ unNick name)
      liftIO $ threadDelay 1000
      ircJoin generalChan
      mainLoop

mainLoop :: Labtech a
mainLoop = forever $ do
  undefined

