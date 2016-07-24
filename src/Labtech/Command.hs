module Labtech.Command where

import Labtech.Command.Types
import Labtech.DB
import Labtech.Help ( help )
import Labtech.IRC.Types
import Labtech.Types

import Control.Monad ( void )
import Control.Monad.IO.Class ( liftIO )
import Data.Bifunctor ( first )
import Text.Megaparsec
import Text.Megaparsec.String

data Command
    = Upload Url Title
    | Help
    | List
    | Get
    | Say String
    deriving (Show, Read)

parseCommand :: CommandEnv String -> Either String Command
parseCommand env
  = first parseErrorPretty $ runParser commandParser name $ commandBody env where

    commandName :: String -> Parser ()
    commandName cname
      = void $ try (string cname <* notFollowedBy (choice [alphaNumChar, symbolChar]))

    name = "irc:" ++ (serverHost . commandServerSpec) env ++ targetName
    targetName = renderTarget (commandTarget env)

    commandParser :: Parser Command
    commandParser = do
      void (try (string "!") <?> "bang")
      choice [uploadCommand, helpCommand, listCommand, sayCommand]

    sayCommand :: Parser Command
    sayCommand = do
      commandName "say"
      skipSome spaceChar
      Say <$> anyChar `manyTill` eof

    uploadCommand :: Parser Command
    uploadCommand = do
      commandName "upload"
      skipSome spaceChar
      url <- anyChar `someTill` spaceChar
      skipMany spaceChar
      p <- anyChar `someTill` eof
      pure $ Upload url p

    helpCommand :: Parser Command
    helpCommand = commandName "help" *> pure Help

    listCommand :: Parser Command
    listCommand = commandName "list" *> pure List

handleCommand :: CommandEnv Command -> Labtech ()
handleCommand (CommandEnv
  { commandBody = command
  , commandTarget = target
  , commandSender = nick
  }) = case command of
    Help -> mapM_ (ircPrivmsg target) help
    Say str -> ircPrivmsg target str
    List -> do
        es <- liftIO queryUploads
        mapM_ (ircPrivmsg target) $ map formatEntry es
    Upload url title -> do
        hasUrl <- liftIO $ dbContains url
        hasTitle <- liftIO $ dbContains title
        if hasUrl then
            ircPrivmsg target $ 
            "Url: " ++ url ++ " already exists in database."
        else if hasTitle then
            ircPrivmsg target $ 
            "Title: \"" ++ title ++ "\" already exists in database."
        else do
            fp <- liftIO $ saveLink url title
            case fp of
                Just f  -> do
                    res <- liftIO $ insertUpload url title f nick
                    ircPrivmsg target res
                Nothing -> ircPrivmsg target $ "Could not save \""
                           ++ title ++ "\" (" ++ url ++ ")"

    _ -> ircPrivmsg target $ "unimplemented command: " ++ show command
