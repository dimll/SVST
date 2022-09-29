import Data.List
import LTS    
import Test.QuickCheck

-- ltsGen :: Gen IOLTS 

-- Generate the number of total transitions
genTransitionsNum :: Gen Integer
genTransitionsNum = do 
    n <- choose (2,4)
    return n 

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


createTTuple :: State -> String -> State -> (State, String,State)
createTTuple s1 str s2 = (s1,str,s2)


-- type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
-- createIOLTS [(1, "?coin", 2), (2, "!tea", 3), (2, "!coffee", 4)]
ltsGen :: Gen IOLTS
ltsGen = do
    transitionsNum <- genTransitionsNum
    statesList <- genStatesList
    
    
    let t1 = (1, "?coin", 2)
    let t2 = (1, "?coin", 3)
    a1 <- elements [t1,t2]
    return (createIOLTS [a1])




main :: IO ()
main = do 
    print "Ex 2 - Start"
    print (counterImpl)
