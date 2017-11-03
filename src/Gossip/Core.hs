module Gossip.Core
  -- ( m
  -- , start
  -- , tick
  -- )
 where

import Control.Lens
import Control.Monad.Random
import Control.Monad.RWS.Strict
import qualified Data.Map as M

import Gossip.Types
import Gossip.Util


placePhrases = M.fromList [(Hotel, [\a b -> a ++ " and " ++ b ++
                                            " were seen checking into the hotel",
                                    \a b -> "Spotted: " ++ a ++ " and " ++ b ++
                                            " starting a romantic rendezvous"]),
                           (School, [\a b -> "What were " ++ a ++ " and " ++
                                             b ++ " doing at school together? Perhaps "
                                            ++ " a little sextra credit?",
                                     \a b -> "Studying hard or hardly studying, eh " ++
                                             a ++ " and " ++ b ++ "?"]),
                           (Bar, [\a b -> "Who was that meeting " ++ a ++
                                  " for drinks? Could be " ++ b ++
                                  " is taking a stumble on the wild side?",
                                  \a b -> a ++ " and " ++ b ++
                                  " were seen at the bar--drowning their sorrows," ++
                                  " or developing new ones?"]),
                           (Store, [\a b -> "What was " ++ b ++ " buying? And why was " ++
                                    a ++ " waiting across the street?",
                                    \a b -> "They say money can't buy happiness, but " ++
                                    a ++ " and " ++ b ++ " sure seem to be trying"])]

m :: PlaceMap
m = M.fromList [ (Hotel, [ (Bar, 4)
                         , (School, 2)
                         , (Store, 2)])
               , (Bar, [ (Store, 4)
                       , (Hotel, 4)])
               , (Store, [ (Hotel, 2)
                         , (School, 4)
                         , (Bar, 4)])
               , (School, [ (Hotel, 2)
                          , (Store, 4)])
               ]





-- tick :: G
-- tick = do
--   cs <- use chars
--   pm <- ask
--   atPlaces

-- tick :: RWS PlaceMap String S ()
-- tick = do
--   cs <- use chars
--   pm <- ask
--   cs' <- cs `forM` \(Character n (p, d)) ->
--             case d of
--               1 -> do
--                 tellChar n p
--                 (newPath, g') <- newDest p pm <$> use gen
--                 gen .= g'
--                 return $ Character n newPath
--               otherwise -> do
--                 return $ Character n (p, d - 1)
--   chars .= cs'
--   return ()

-- start = makeS 1 [Character "A" (Hotel, 4),
--                  Character "B" (Store, 4)]
