module Rng where 

import System.Random
import Data.Map (singleton,insert,elems,Map,(!))

shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle g [] = ([], g)
shuffle g l  = (elems x, y)
  where
    (x, y) = foldl step (singleton 0 (head l), g) (zip [1..] (tail l))

    step :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
    step (m, g) (i, x) = let (j, g') = randomR (0, i) g in ((insert j x . insert i (m ! j)) m, g')
