module Gossip.Core
  ( m
  , start
  , tick
  )
 where

import Control.Lens
import Control.Monad.RWS.Strict
import System.Random

import Gossip.Types
import Gossip.Util

hotel = Place "Hotel" [\a b -> a ++ " and " ++ b ++
                               " were seen checking into the hotel",
                       \a b -> "Spotted: " a ++ " and " ++ b ++ " starting a" ++
                               " romantic rendezvous"]
-- school = Place "School" [\a b ->

m :: PlaceMap
m = [ (Hotel, (Bar, 4))
    , (Hotel, (School, 2))
    , (Hotel, (Store, 2))
    , (Bar, (Store, 4))
    , (Bar, (Hotel, 4))
    , (Store, (Hotel, 2))
    , (Store, (School, 4))
    , (Store, (Bar, 4))
    , (School, (Hotel, 2))
    , (School, (Store, 4))
    ]


tellChar :: String -> Place -> RWS PlaceMap String S ()
tellChar n pl = tell $ n ++ " is at " ++ show pl ++ "\n"

newDest :: (RandomGen g) => Place -> PlaceMap -> g -> (Path, g)
newDest pl pm g = ch g $ lk pm pl

tick :: RWS PlaceMap String S ()
tick = do
  cs <- use chars
  pm <- ask
  cs' <- cs `forM` \(Person n (p, d)) ->
            case d of
              1 -> do
                tellChar n p
                (newPath, g') <- newDest p pm <$> use gen
                gen .= g'
                return $ Person n newPath
              otherwise -> do
                return $ Person n (p, d - 1)
  chars .= cs'
  return ()

start = makeS 1 [Person "A" (Hotel, 4),
                 Person "B" (Store, 4)]
