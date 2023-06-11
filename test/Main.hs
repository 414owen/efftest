{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (throwIO)
import Data.Bifunctor (first)
import qualified Act

data ActTest
  = Fetch { fetchUrl :: String, fetchRes :: String }
  | Print String
  deriving Show

-- instance Show ActTest where
--   show a = case a of
--     Fetch { url } -> "Fetch " <> url
--     Print str -> "Print " <> str

runTest :: Show a => Act.Act a -> [ActTest] -> Either String a
runTest action testActions = case go action testActions of
  Left err -> Left err
  Right (_, _ : _) -> Left $ "Expected more actions: " <> show testActions
  Right (a, []) -> Right a

  where
    go :: Act.Act a -> [ActTest] -> Either String (a, [ActTest])
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
    acts = Act.Bind (Act.Fetch "owen.cafe") Act.Print

    expected :: [ActTest]
    expected = [Fetch { fetchUrl = "owen.cafe", fetchRes = "hello" }, Print "hello"]
