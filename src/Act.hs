{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Act
  ( Act(..)
  , interpretIO
  ) where

import qualified Data.ByteString.Lazy.UTF8 as BSL
import           Network.HTTP.Conduit (simpleHttp)

data Act a where
  Fetch :: String -> Act String
  Print :: String -> Act ()

  -- Could we implement these in terms of Bind? Sure!
  -- But that means we can't parallelize these
  FMap :: (b -> a) -> Act b -> Act a
  Pure :: a -> Act a
  Ap :: (Act (b -> a)) -> Act b -> Act a

  Bind :: Act b -> (b -> Act a) -> Act a

instance Functor Act where
  fmap = FMap

instance Applicative Act where
  pure = Pure
  (<*>) = Ap

instance Monad Act where
  (>>=) = Bind

instance Show (Act a) where
  show a' = unlines $ showAct a'

sep :: String
sep = "  "

indent :: [String] -> [String]
indent = fmap (sep <>)

showAct :: Act a -> [String]
showAct a = case a of
  Fetch url -> ["Fetch " <> url]
  Print str -> ["Print " <> str]
  Bind l _ -> "Bind" : sep <> show l : [sep <> "<fn>"]
  FMap _ _-> "Fmap" : sep <> "<fn> " : indent (showAct a)
  Pure _ -> ["Pure"]
  Ap _ _ -> "Ap" : sep <> "fn" : indent (showAct a)

interpretIO :: Act a -> IO a
interpretIO m = case m of
  Fetch url -> BSL.toString <$> simpleHttp url
  Print str -> putStr str
  Bind act f -> do
    a <- interpretIO act
    interpretIO $ f a
  FMap f a -> f <$> interpretIO a
  Pure a -> pure a
  Ap f a -> interpretIO f <*> interpretIO a
