module Labtech.IRC where

import Labtech.IRC.Types

import Control.Monad.Reader
import Control.Concurrent.MVar ( MVar, newMVar, takeMVar, putMVar )
import Data.Bifunctor ( first )
import Text.Printf ( printf )
import Text.Megaparsec
import Text.Megaparsec.String

write :: (IRCConn c) => c -> String -> String -> IO ()
write c a b = do
    ircPrint c $ a ++ " " ++ b ++ "\r\n"
    printf "> %s %s\n" a b

makeIrcEnv :: (IRCConn h) => h -> ServerSpec -> IO IrcEnv
makeIrcEnv conn _ = do
  mh <- newMVar conn
  let withH = _withH mh
  let _user (Username username) (RealName realname) = withH $ \h ->
        write h "USER" $ username ++ " 0 * :" ++ realname
  let _msg target msg
        = withH $ \h -> write h "PRIVMSG" $ renderTarget target ++ " :" ++ msg

  pure IrcEnv
    { _nickE = \(Nick nick) -> withH $ \h -> write h "NICK" nick
    , _joinE = \(Channel chan) -> withH $ \h -> write h "JOIN" chan
    , _pongE = \(Ping ping) -> withH $ \h -> write h "PONG" $ ':':ping
    , _nextE = _next
    , _userE = _user
    , _privmsgE = _msg
    }
  where
    _withH :: MonadIO m => MVar c -> (c -> m a) -> m a
    _withH mh m = do
      h <- liftIO $ takeMVar mh
      x <- m h
      liftIO $ putMVar mh h
      pure x

    _next = do
      line <- ircGetLine conn
      liftIO $ putStrLn $ "< " ++ line
      case parseMessage line of
        Left e -> do
          putStr e
          _next
        Right x -> pure x

-- | Parses an IRC message.
parseMessage :: String -> Either String Message
parseMessage = first parseErrorPretty . runParser messageParser "irc" where
  messageParser :: Parser Message
  messageParser = choice [ privmsg, ping, nickInUse ] where
    privmsg :: Parser Message
    privmsg = do
      origin <- try $ msgorigin <* many spaceChar <* string' "privmsg"
      skipMany spaceChar
      target <- msgtarget
      void $ many spaceChar
      void $ string ":"
      msg <- anyChar `manyTill` eof
      pure $ Privmsg origin target msg

    ping :: Parser Message
    ping = do
      void $ try $ string' "ping"
      skipMany spaceChar
      void $ string ":"
      msg <- anyChar `manyTill` eof
      pure $ Pingmsg (Ping msg)

    nickInUse :: Parser Message
    nickInUse = do
      void $ try $ do
        void $ string' ":"
        void (anyChar `manyTill` spaceChar)
        void $ skipMany spaceChar
        void $ string "433"
      pure NickInUse

  msgtarget :: Parser MessageTarget
  msgtarget = (ChannelTarget <$> channel) <|> (NickTarget <$> nick) where
    channel
      = fmap Channel $ (++) <$> try (string "#") <*> (anyChar `someTill` spaceChar)
    nick
      = fmap Nick $ (anyChar `someTill` spaceChar)

  msgorigin :: Parser MessageOrigin
  msgorigin = do
    void $ string ":"
    nick <- anyChar `someTill` string "!"
    host <- anyChar `someTill` spaceChar
    pure $ MessageOrigin (Nick nick) host
