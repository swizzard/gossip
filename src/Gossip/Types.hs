{-# LANGUAGE TemplateHaskell #-}

module Gossip.Types
  ( Character(..)
  , characterName
  , characterPath
  , G
  , runG
  , Path(..)
  , Place(..)
  , PlaceMap(..)
  , S
  ) where

import Control.Lens
import Control.Lens.TH
import Control.Monad.Random
import Control.Monad.State
import qualified Data.Map as M
import System.Random (StdGen, getStdGen, mkStdGen)

import Gossip.Util (ch, lk)

data Place = Hotel | School | Bar | Store deriving (Eq, Ord, Show)
type PlacePhrase = String -> String -> String

type Path = (Place, Int)

type PlaceMap = M.Map Place [Path]

data Character = Character { _characterName :: String
                           , _characterPath :: Path
                           } deriving (Eq, Show)
makeLenses ''Character

data Season = Spring | Summer | Fall | Winter deriving (Enum, Eq, Ord, Show)

data S = S { _chars :: [Character]
           , _placePhrases :: M.Map Place [PlacePhrase]
           , _iteration :: Int
           , _season :: Season
           , _placeMap :: PlaceMap
           , _gossip :: String
           }
makeLenses ''S

instance Show S where
  show s = "( characters: " ++ views chars show s ++ ", " ++
           "iteration: " ++ views iteration show s ++ ", " ++
           "season: " ++ views season show s ++ ")"

type G = StateT S (Rand StdGen)

runG :: Int -> G () -> String
runG = undefined -- for now

writeLog :: String -> G ()
writeLog s = gossip %= (++ s ++ "\n")

logLines :: G Int
logLines = uses gossip (length . lines)

logWords :: G Int
logWords = uses gossip (length . words)

incIteration :: G ()
incIteration = iteration += 1

changeSeason :: Season -> G ()
changeSeason s = season .= s


atPlaces :: [Character] -> M.Map Place [Character]
atPlaces cs = foldl f M.empty cs where
  f m c | c ^. characterPath . _2 == 0 = M.insertWith (++)
                                       (c ^. characterPath . _1) [c] m
        | otherwise = m

placePairs :: M.Map Place [Character] -> M.Map Place [Character]
placePairs = M.filter ((== 2) . length)
