module Main where

-- Import necessary modules
import LC
import Test.HUnit

-- Example lambda expression that normalizes in one try
lambdaIdentity :: LambdaExpress
lambdaIdentity =
  App (Abs "x" (Var "x")) (Var "Otter")

-- Example lambda expression that normalizes in 3 tries
lambdaBinaryFunction :: LambdaExpress
lambdaBinaryFunction =
  App
    (App
      (App
        (Abs "f"
          (Abs "x"
            (Abs "y"
              (App (App (Var "f") (Var "x")) (Var "y"))
            )
          )
        )
        (Abs "a"
          (Abs "b"
            (App (App (Var "+") (Var "a")) (Var "b"))
          )
        )
      )
      (Var "3")
    )
    (Var "4")


-- Example lambda expression that doesn't normalize
lambdaNoNormal :: LambdaExpress
lambdaNoNormal =
  App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (App (Var "x") (Var "x")))

testNormalization :: Test
testNormalization =
  TestList
    [ TestCase $
        assertEqual
          "Test 1: lambdaIdentity with 1 step"
          "Otter"
          (isNormalized (tnf lambdaIdentity 1)),
      TestCase $
        assertEqual
          "Test 2: lambdaIdentity with 3 steps"
          "Otter"
          (isNormalized (tnf lambdaIdentity 3)),
      TestCase $
        assertEqual
          "Test 3: lambdaIdentity with 5 steps"
          "Otter"
          (isNormalized (tnf lambdaIdentity 5)),
      TestCase $
        assertEqual
          "Test 4: lambdaBinaryFunction with 1 step"
          ("No normalization found with resources provided: "
          ++ "(lambda x.(lambda y.(lambda a.(lambda b.+ a b)) x y)) 3 4")
          (isNormalized (tnf lambdaBinaryFunction 1)),
      TestCase $
        assertEqual
          "Test 5: lambdaBinaryFunction with 3 steps"
          ("No normalization found with resources provided: "
          ++ "(lambda a.(lambda b.+ a b)) 3 4")
          (isNormalized (tnf lambdaBinaryFunction 3)),
      TestCase $
        assertEqual
          "Test 6: lambdaBinaryFunction with 5 steps"
          "+ 3 4"
          (isNormalized (tnf lambdaBinaryFunction 5)),
      TestCase $
        assertEqual
          "Test 7: lambdaNoNormal with 1 step"
          ("No normalization found with resources provided: "
          ++ "(lambda x.x x) (lambda x.x x)")
          (isNormalized (tnf lambdaNoNormal 1)),
      TestCase $
        assertEqual
          "Test 8: lambdaNoNormal with 3 steps"
          ("No normalization found with resources provided: "
          ++ "(lambda x.x x) (lambda x.x x)")
          (isNormalized (tnf lambdaNoNormal 3)),
      TestCase $
        assertEqual
          "Test 9: lambdaNoNormal with 5 steps"
          ("No normalization found with resources provided: "
          ++ "(lambda x.x x) (lambda x.x x)")
          (isNormalized (tnf lambdaNoNormal 5))
    ]

-- Function to check normalization for a list of terms
checkNormalizationList :: LambdaExpress -> [Int] -> IO ()
checkNormalizationList expr ns =
  mapM_
    ( \n -> do
        printOriginalAndResult expr n
        putStrLn ""
    )
    ns

-- Main function to execute tests and checks
main :: IO ()
main = do
  checkNormalizationList lambdaIdentity [1, 3, 5]
  checkNormalizationList lambdaBinaryFunction [1, 3, 5]
  checkNormalizationList lambdaNoNormal [1, 3, 5]
  _ <- runTestTT $ TestList [testNormalization]
  return ()