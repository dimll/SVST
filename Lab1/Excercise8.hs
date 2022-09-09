-- Time spent: 150 minutes

import Data.Function
import Data.List

data Boy = Matthew | Peter | Jack | Arnold | Carl
  deriving (Eq, Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

accuses :: Boy -> Boy -> Bool
accuses Matthew b = b /= Carl && b /= Matthew
accuses Peter b = b == Matthew || b == Jack
accuses Jack b = not (b /= Carl && b /= Matthew) && not (b == Matthew || b == Jack)
accuses Arnold b = ((b /= Carl && b /= Matthew) && not (b == Matthew || b == Jack)) || (not (b /= Carl && b /= Matthew) && (b == Matthew || b == Jack))
accuses Carl b = not (((b /= Carl && b /= Matthew) && not (b == Matthew || b == Jack)) || (not (b /= Carl && b /= Matthew) && (b == Matthew || b == Jack)))

accusers :: Boy -> [Boy]
accusers b = filter (`accuses` b) boys

accusations :: Boy -> [Boy]
accusations b = filter (accuses b) boys

-- Sort all accusations by their length and retrieve the list of accusers with the most boys.
-- Below is the source for sorting the list by nested list length
-- https://stackoverflow.com/questions/2307893/sorting-lists-of-lists-in-haskell
honest :: [Boy]
honest = last $ last (groupBy ((==) `on` length) $ sortBy (compare `on` length) (map accusers boys))

guilty :: [Boy]
guilty = foldr1 intersect (map accusations honest)
