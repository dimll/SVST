import Data.List
import LTS    
import Test.QuickCheck

-- ltsGen :: Gen IOLTS 

-- Generate boolean value 
genBool :: Gen Bool 
genBool = do 
    b <- arbitrary
    return b

-- -- Generate the number of total transitions
-- genTransitionsNum :: Gen Integer
-- genTransitionsNum = do 
--     n <- choose (2,4)
--     return n 

-- Generate list of States 
genStatesList :: Gen [State]
genStatesList = do 
    size <- choose(2,4)
    res <- vectorOf size arbitrary
    return (map abs res)


-- Generates input label from a list of input labels
genInputLabel :: Gen Label
genInputLabel = do
    let inLabels = ["?coin", "?button", "?kick"]
    inLabel <- elements inLabels
    return inLabel 

-- Generates output label from a list of output labels
genOutputLabel :: Gen Label
genOutputLabel = do
    let outLabels = ["!coffee", "!tea", "!juice"]
    outLabel <- elements outLabels
    return outLabel 

genTransition :: Gen LabeledTransition
genTransition = do 
    l1 <- genStatesList
    s1 <- elements l1
    s2 <- elements l1
    actionType <- genBool 
    action <- if (actionType) 
                then genInputLabel
                else genOutputLabel
    return (s1,action,s2)
    
genTransitions :: Gen [LabeledTransition]
genTransitions = do 
    size <- choose(2,4)
    res <- vectorOf size genTransition
    return res


-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
-- createIOLTS [(1, "?coin", 2), (2, "!tea", 3), (2, "!coffee", 4)]
ltsGen :: Gen IOLTS
ltsGen = do
    t <- genTransitions
    return (createIOLTS t)

main :: IO ()
main = do 
    print "Ex 2 - Start"
    print (counterImpl)
