{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.Trans.Writer.Lazy (Writer, execWriter, tell)
import Control.Exception (throwIO)
import Data.Bifunctor (first)
import qualified Act

data ActTest'
  = Fetch { fetchUrl :: String, fetchRes :: String }
  | Print String
  deriving Show

-- TODO use dlist?
type ActTest = Writer [ActTest'] ()

expFetch :: String -> String -> ActTest
expFetch url res = tell $ pure $ Fetch url res

expPrint :: String -> ActTest
expPrint = tell . pure . Print

runTest :: Show a => Act.Act a -> ActTest -> Either String a
runTest action testActions = case go action (execWriter testActions) of
  Left err -> Left err
  Right (_, _ : _) -> Left $ "Expected more actions: " <> show testActions
  Right (a, []) -> Right a

  where
    go :: Act.Act a -> [ActTest'] -> Either String (a, [ActTest'])
    go act testActs = case (act, testActs) of
      (Act.FMap f a, _) -> first f <$> go a testActs
      (Act.Bind left right, _) -> do
        (res, rest) <- go left testActs
        go (right res) rest
      (Act.Fetch url, Fetch turl res : rest)
        | url == turl -> Right (res, rest)
      (Act.Print str1, Print str2 : rest)
        | str1 == str2 -> Right ((), rest)
      (got, expected) -> Left 
        $ "Expected " <> show expected <> ", got " <> show got

eitherToIO :: Either String a -> IO a
eitherToIO e = case e of
  Left err -> throwIO $ userError err
  Right a -> pure a

main :: IO ()
main = eitherToIO $ runTest acts expected
  where
    acts :: Act.Act ()
    acts = do
      res <- Act.Fetch "owen.cafe"
      Act.Print res

    expected :: ActTest
    expected = do
      expFetch "owen.cafe" "hello"
      expPrint "hello"
