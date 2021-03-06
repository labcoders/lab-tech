{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan

import Labtech
import qualified Labtech.InternalMessaging as IM
import Labtech.IRC.Servers ( servers )
import qualified Labtech.Web as Web

main :: IO ()
main = do
  regChan <- newChan
  _ <- forkIO (IM.mainLoop regChan)
  mapM_ (forkIO . runOnServer regChan) servers
  _ <- forkIO $ Web.main regChan
  putStrLn "press return to quit"
  _ <- getLine
  pure ()
