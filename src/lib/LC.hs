module LC where

data LambdaExpress
  = Var String
  | Abs String LambdaExpress
  | App LambdaExpress LambdaExpress
  deriving (Eq, Show)

printLambdaExpress :: LambdaExpress -> String
printLambdaExpress (Var x)     = x
printLambdaExpress (Abs x t)   = "(" ++ "lambda " ++ x ++ "." ++ printLambdaExpress t ++ ")"
printLambdaExpress (App t1 t2) = printLambdaExpress t1 ++ " " ++ printLambdaExpress t2

substitution :: LambdaExpress -> String -> LambdaExpress -> LambdaExpress
substitution (Var y) x s       = if x == y then s else Var y
substitution (Abs y t1) x s    = if x == y then Abs y t1 else Abs y (substitution t1 x s)
substitution (App t1 t2) x s   = App (substitution t1 x s) (substitution t2 x s)

reduction :: LambdaExpress -> LambdaExpress
reduction (App (Abs x t1) t2) = substitution t1 x t2
reduction (App t1 t2)         = App (reduction t1) t2
reduction (Abs x t)           = Abs x (reduction t)
reduction t                   = t

-- Perform reduction n times
reduceNTimes :: LambdaExpress -> Int -> LambdaExpress
reduceNTimes expr 0 = expr
reduceNTimes expr n = reduceNTimes (reduction expr) (n - 1)

isNormalized :: LambdaExpress -> Either String LambdaExpress
isNormalized term =
  let reducedTerm = LC.reduction term
  in if reducedTerm == term
       then Right term -- Term is in normal form
       else Left "Normalization not possible with the resources provided"
