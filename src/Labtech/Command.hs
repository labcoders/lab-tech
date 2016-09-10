{-# LANGUAGE ViewPatterns #-}

module Labtech.Command where

import Labtech.Command.Types
import Labtech.DB
import Labtech.DB.Queries
import Labtech.DB.Types
import Labtech.FS ( saveLink )
import Labtech.Help ( help )
import Labtech.IRC.Types
import Labtech.Types

import Control.Monad ( void )
import Control.Monad.IO.Class ( liftIO )
import Data.Bifunctor ( first )
import Text.Megaparsec
import Text.Megaparsec.String

renderListTarget :: ListTarget -> String
renderListTarget t = case t of
  ListUploads -> "uploads"
  ListIdeas -> "ideas"

data Command
    -- | Upload the contents of an internet resource to the database.
    = Upload
      Url -- ^ The URL whose contents is recorded to disk.
      Title -- ^ A unique name for the upload.
    -- | Show the labtech help.
    | Help
    -- | List data from the database.
    | List
      ListTarget -- ^ The type of data to list.
    -- | Delete something from the database.
    | Delete
      ListTarget -- ^ The type of data to delete.
      Int -- ^ The index of the datum.
    | Get
    -- | Make labtech say an arbitrary message.
    | Say
      String -- ^ The text to say.
    -- | Record an idea to the database.
    | Idea
      String -- ^ The idea to record.
    -- | Try to rename labtech to the first available given nick.
    | Renick
      [Nick]
      -- ^ The nicks to try to rename labtech to. An empty list of nicks will
      -- use the nick list from the server specification associated with the
      -- worker thread that processes the message.
    deriving (Show)

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
      choice
        [ uploadCommand
        , helpCommand
        , listCommand
        , sayCommand
        , ideaCommand
        , deleteCommand
        , renickCommand
        ]

    sayCommand :: Parser Command
    sayCommand = do
      commandName "say"
      skipSome spaceChar
      Say <$> anyChar `manyTill` eof

    deleteCommand :: Parser Command
    deleteCommand = do
      commandName "delete"
      skipSome spaceChar
      tgt <- listTarget
      skipMany spaceChar
      i <- numberChar `someTill` eof
      pure $ Delete tgt (read i)

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

    renickCommand :: Parser Command
    renickCommand = do
      commandName "nick"
      skipSome spaceChar
      nicks <- fmap (map Nick) $ some alphaNumChar `sepBy` some spaceChar
      pure $ Renick nicks

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

handleCommand
  :: MonadLabIrcIO m
  => ConnectionPool
  -> CommandEnv Command
  -> m ()
handleCommand
  pool
  (CommandEnv
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
              map formatIdea <$> runDB pool listIdeas
            ListUploads -> do
              putStrLn "listing uploads"
              map formatEntry <$> runDB pool queryUploads
      if null f
      then ircPrivmsg target $ "No " ++ renderListTarget s
      else mapM_ (ircPrivmsg target) f

    Delete (deleteQueryFor -> l) i -> do
        b <- liftIO $ runDB pool $ deleteFrom l i
        ircPrivmsg target b

    Idea i -> do
        hasIdea <- liftIO $ runDB pool $ ideaTableContains i
        if hasIdea then
            ircPrivmsg target $
            "Idea already exists in database."
        else do
            res <- liftIO $ runDB pool $ insertIdea i nick
            ircPrivmsg target res
    Upload url title -> do
        hasUrl <- liftIO $ runDB pool $ uploadTableContains url
        hasTitle <- liftIO $ runDB pool $ uploadTableContains title
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
                    res <- liftIO $ runDB pool $ insertUpload url title f nick
                    ircPrivmsg target res
                Nothing -> ircPrivmsg target $ "Could not save \""
                           ++ title ++ "\" (" ++ url ++ ")"

    Renick explicitNicks -> do
      specNicks <- serverNicks <$> labGetSpec
      labPreferNicks $ if null explicitNicks then specNicks else explicitNicks
      labRenick

    _ -> ircPrivmsg target $ "unimplemented command: " ++ show command
