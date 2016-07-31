module Labtech.Async where

import Control.Concurrent.Async
import Control.Exception

-- | Runs an IO action, catching all exceptions, and repeating the IO action
-- until it succeeds. The given function can inspect the exception and decide
-- to abort the restart loop by producing some value. If it produces no value,
-- then another loop is performed.
restarting :: (SomeException -> IO (Maybe b)) -> IO a -> IO (Either b a)
restarting f m = do
  a <- async m
  r <- waitCatch a
  case r of
    Left err -> do
      b <- f err
      case b of
        Nothing -> restarting f m
        Just e -> pure (Left e)
    Right x -> pure (Right x)

-- | Repeats an IO action until it completes without raising any exceptions.
restarting' :: IO a -> IO a
restarting' m = right <$> restarting f m where
  f _ = pure Nothing
  right e = case e of
    Right x -> x
    Left _ -> error "restarting': Left branch is impossible"

