module LC where

-- Define the data type for lambda calculus expressions
data LambdaExpress
  = Var String -- Variable
  | Abs String LambdaExpress -- Abstraction (λx.t)
  | App LambdaExpress LambdaExpress -- Aplication (t1 t2)
  deriving (Eq, Show)

-- Function to convert a LambdaExpress to a string representation
printLambdaExpress :: LambdaExpress -> String
printLambdaExpress (Var x)     = x
printLambdaExpress (Abs x t)   =
  "(" ++ "lambda " ++ x ++ "." ++ printLambdaExpress t ++ ")"
printLambdaExpress (App t1 t2) =
  printLambdaExpress t1 ++ " " ++ printLambdaExpress t2

-- Substitution function to replace variables in a LambdaExpress
substitution :: LambdaExpress -> String -> LambdaExpress -> LambdaExpress
substitution (Var y) x s       =
  if x == y then s else Var y
substitution (Abs y t1) x s    =
  if x == y then Abs y t1 else Abs y (substitution t1 x s)
substitution (App t1 t2) x s   =
  App (substitution t1 x s) (substitution t2 x s)

-- Reduction function to simplify LambdaExpress ONE TIME
reduction :: LambdaExpress -> LambdaExpress
reduction (App (Abs x t1) t2) = substitution t1 x t2
reduction (App t1 t2)         = App (reduction t1) t2
reduction (Abs x t)           = Abs x (reduction t)
reduction t                   = t

-- Perform reduction n times on a LambdaExpress
tnf :: LambdaExpress -> Int -> LambdaExpress
tnf expr 0 = expr
tnf expr n = tnf (reduction expr) (n - 1)

-- Check if a LambdaExpress is in normal form
isNormalForm :: LambdaExpress -> Bool
isNormalForm (Var _)     = True
isNormalForm (Abs _ t)   = isNormalForm t
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App t1 t2) = isNormalForm t1 && isNormalForm t2

-- If LambdaExpress is not in normal form,
-- put a message before returning reduction
isNormalized :: LambdaExpress -> String
isNormalized t =
  if isNormalForm t
    then printLambdaExpress t
    else
      "No normalization found with resources provided: " ++ printLambdaExpress t

-- Print the original expression,
-- resources available, and its result after normalization
printOriginalAndResult :: LambdaExpress -> Int -> IO ()
printOriginalAndResult originalExpr n = do
  putStrLn $ "Number of resources: " ++ show n
  putStrLn $ "Original expression: " ++ printLambdaExpress originalExpr
  let result = isNormalized (tnf originalExpr n)
  putStrLn $ "Normalized expression: " ++ result