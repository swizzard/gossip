{-# LANGUAGE TemplateHaskell #-}
module Gossip.Types
  ( Path(..)
  , Person(..)
  , name
  , path
  , Place(..)
  , placeName
  , placePhrases
  , PlaceMap(..)
  , S
  , gen
  , chars
  , makeS
  , makeSIO
  ) where

import Control.Lens.TH
import System.Random (StdGen, getStdGen, mkStdGen)

import Gossip.Util (ch, lk)

data Place' = Bar | Hotel | School | Store

type PlacePhrase = Person -> Person -> String
data Place = { _placeName :: String
             , _placePhrases :: [PlacePhrase]
             }
mkLenses ''Place

atPlace :: Person -> Person -> Place -> StdGen -> (String, StdGen)
atPlace p1 p2 pl g = let (f, g') = ch g pl ^. placePhrases
                      in (f p1 p2, g')

type Path = (Place, Int)

type PlaceMap = [(Place', Path)]

data Person = Person { _name :: String
                     , _path :: Path
                     } deriving (Eq, Show)
makeLenses ''Person


data S = S { _gen :: StdGen
           , _chars :: [Person]
           } deriving (Show)
makeLenses ''S

makeS :: Int -> [Person] -> S
makeS n cs = S (mkStdGen n) cs

makeSIO :: [Person] -> IO S
makeSIO cs = S <$> getStdGen <*> return cs
