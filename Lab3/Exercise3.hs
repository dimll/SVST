import Lecture3

getFalseValues :: Form -> [Valuation]
getFalseValues f = filter (\v -> not $ evl v f) (allVals f)

cnf :: Form -> Form
cnf f = (map Neg (filter (\v -> not $ evl v f) (allVals f)))