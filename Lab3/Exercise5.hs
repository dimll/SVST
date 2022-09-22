import SetOrd
import Lecture3
import Exercise4
import Data.Char

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

--Property 1: Are all atoms included in the output
propAllAtomsAreIncluded :: Form -> Bool 
propAllAtomsAreIncluded f = all (\x -> inSet (Prop x) (sub f)) (propNames f)

--Property 2: All connectives of form should be present in the subforms
stripForm :: Form -> [Token] 
stripForm f = filter (\x -> x /= TokenOP && x /= TokenCP && not (isDigit (last $ show x))) (lexer (showLst [f]))
