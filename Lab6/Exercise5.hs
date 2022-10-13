module Exercise5 where
import Test.QuickCheck
import System.Random
import Exercise2
import Exercise3
import Exercise4
import SetOrd
import Data.List

-- infixr 5 @@ defines a function composition RoS. If S=R then RoR=(R^2)
infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--transitiveAdditions finds what should be added to the relation to make it transitive
transitiveAdditions x y = [(x1, y2) | (x1, x2) <- x, (y1, y2) <- y, x2 == y1]
--maketransitive adds the additional elements to make the relation transitive
--Ex: maketransitive [(1,2),(2,3),(2,2)]
maketransitive relR = nub(relR ++ transitiveAdditions relR relR)

--For transitive closure we will define R that is transitive. Then apply R union R^n untill R does not change anymore
--Ex:  trClos [(1,2),(2,3),(2,4)]
trClos ::  Ord a => Rel a -> Rel a 
trClos rel 
    | old_closure == rel = rel      --if old R is equal to R^n we stop
    | otherwise = trClos old_closure    --if old R is not equal to R^n we continue with 1 more @@ (1 more function composition)

        where new_closure = rel @@ rel      -- rel @@ rel = R@@R = R^2
              closure =rel
              old_closure = closure `union` new_closure  --rel Union R^n

