module Labtech.Command where

import qualified Data.Char as C

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

data ListTarget
  = ListUploads
  | ListIdeas
  deriving (Eq, Ord, Read, Show)

renderListTarget :: ListTarget -> String
renderListTarget t = case t of
  ListUploads -> "uploads"
  ListIdeas -> "ideas"

data Command
    = Upload Url Title
    | Help
    | List ListTarget
    | Get
    | Say String
    | Idea String
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
      choice [uploadCommand, helpCommand, listCommand, sayCommand, ideaCommand]

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

    ideaCommand :: Parser Command
    ideaCommand = do
      commandName "idea"
      skipSome spaceChar
      idea <- anyChar `someTill` eof
      pure $ Idea idea

    listTarget :: Parser ListTarget
    listTarget = choice
      [ string "uploads" *> pure ListUploads
      , string "ideas" *> pure ListIdeas
      ]

    helpCommand :: Parser Command
    helpCommand = commandName "help" *> pure Help

    listCommand :: Parser Command
    listCommand = do
      commandName "list" *> skipSome spaceChar *> (List <$> listTarget) <* eof

handleCommand :: CommandEnv Command -> Labtech ()
handleCommand (CommandEnv
  { commandBody = command
  , commandTarget = target
  , commandSender = nick
  }) = case command of
    Help -> mapM_ (ircPrivmsg target) help
    Say str -> ircPrivmsg target str
    List s -> do
      f <- liftIO $ case s of
            ListIdeas -> do
              putStrLn "listing ideas"
              listIdeas
            ListUploads -> do
              putStrLn "listing uploads"
              map formatEntry <$> queryUploads
      if null f
      then ircPrivmsg target $ "No " ++ renderListTarget s
      else mapM_ (ircPrivmsg target) f

    Idea i -> do
        hasIdea <- liftIO $ ideaTableContains i
        if hasIdea then
            ircPrivmsg target $
            "Idea:already exists in database."
        else do
            res <- liftIO $ insertIdea i nick
            ircPrivmsg target res
    Upload url title -> do
        hasUrl <- liftIO $ uploadTableContains url
        hasTitle <- liftIO $ uploadTableContains title
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
