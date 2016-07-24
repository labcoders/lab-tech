{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent ( forkIO )

import Labtech
import Labtech.IRC.Servers ( servers )

main :: IO ()
main = do
  mapM_ (forkIO . runOnServer) servers
  putStrLn "press return to quit"
  _ <- getLine
  pure ()
