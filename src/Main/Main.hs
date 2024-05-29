module Main where

import LC

printOriginalAndResult :: LambdaExpress -> Int -> IO ()
printOriginalAndResult originalExpr n = do
  putStrLn $ "Original expression: " ++ printLambdaExpress originalExpr

  case isNormalized $ reduceNTimes originalExpr n of
    Right normalizedTerm -> do
      putStrLn $ "Normalized expression: " ++ printLambdaExpress normalizedTerm
    Left errorMessage ->
      putStrLn errorMessage

-- Addition function: \x -> \y -> x + y
additionFunction :: LambdaExpress
additionFunction = Abs "x" (Abs "y" (App (App (Var "+") (Var "x")) (Var "y")))

-- Application: (\x -> \y -> x + y) 3 4
testCase2 :: LambdaExpress
testCase2 = App (App additionFunction (Var "3")) (Var "4")


main :: IO ()
main = do
  printOriginalAndResult testCase2 2