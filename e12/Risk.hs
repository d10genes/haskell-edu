{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List
import Control.Arrow ((&&&), (***))

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
     deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle b@Battlefield {attackers, defenders} = do
  -- d <- die
  let (aNum, dNum) = numbAvailable b
  ats <- replicateM aNum die
  dts <- replicateM dNum die
  -- return (ats, dts)
  let x = losses (ats, dts)
  return $ applyLosses x b
  -- return $ fmap unDV ats
  -- return $ unDV d

data Player = Attack | Defend
     deriving (Show, Eq)

numbAvailable Battlefield {attackers, defenders} = (min attackers 3, min defenders 2)

losses' :: Ord b => ([b], [b]) -> [Player]
losses' = uncurry (zipWith comp) . (sortDesc *** sortDesc)
  where comp l r | l > r = Attack
                 | otherwise = Defend

-- losses :: Ord b => ([b], [b]) -> [Player]
losses tup = (as &&& ds) v
  where v = losses' tup
        as = length . filter (== Defend)
        ds = length . filter (== Attack)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

applyLosses :: (Army, Army) -> Battlefield -> Battlefield
applyLosses (a, d) (Battlefield {attackers, defenders}) = Battlefield {attackers= attackers -a, defenders= defenders-d}

done :: Battlefield -> Bool
done b = (attackers b < 2) || (defenders b < 1)

invade :: Battlefield -> Rand StdGen Battlefield
invade b = if done b
     then return b
     else battle b >>= invade

b = battle (Battlefield {attackers=6,defenders=7})
c = (Battlefield {attackers=6,defenders=7})

f = do
    d <- die
    return $ unDV d
