
data Term = 
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term 
  | TmPred Term
  | TmIsZero Term
  | TmWrong
  deriving (Show)

isnumerical :: Term -> Bool
isnumerical TmZero     = True
isnumerical (TmSucc t) = isnumerical t
isnumerical _          = False

isval :: Term -> Bool
isval TmTrue  = True
isval TmFalse = True
isval t       = isnumerical t

eval1 :: Term -> Term
eval1 (TmIf TmTrue t2 _)    = t2
eval1 (TmIf TmFalse _ t3)   = t3
eval1 (TmIf t1 t2 t3)       = TmIf (eval1 t1) t2 t3
eval1 (TmSucc t)            = TmSucc (eval1 t)
eval1 (TmPred TmZero)       = TmZero
eval1 (TmPred (TmSucc t))   = t
eval1 (TmPred t)            = TmPred (eval1 t)
eval1 (TmIsZero TmZero)     = TmTrue
eval1 (TmIsZero (TmSucc _)) = TmFalse
eval1 (TmIsZero t)          = TmIsZero (eval1 t)
eval1 _                     = TmWrong

main :: IO() 
main = do
  print (isnumerical (TmSucc TmZero))
  print (isval TmTrue)
  print (isval (TmSucc TmZero))
  print (eval1 (TmIf TmTrue (TmSucc TmZero) TmZero))

