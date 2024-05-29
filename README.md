# Lambda Calculus Expressions and Reductions

Amelia Hoyos y Valentina Vasquez. 

## System Information
- **Operating System Version:** [ Aquí falta ]
- **GHC Version:** [Aquí falta ]
- **Cabal Version:** [Aquí falta ]

## Overview
This repository contains Haskell code for representing and manipulating lambda calculus expressions. It includes functionality for printing lambda expressions, performing substitutions, reducing expressions, and reducing expressions multiple times. Additionally, the repository includes unit tests for validating the correctness of these functions.

## Modules
### LC
- **Data Types:**
  - `LambdaExpress` for representing lambda calculus expressions (variables, abstractions, and applications).
- **Functions:**
  - `printLambdaExpress` for converting lambda expressions to their string representation.
  - `substitution` for performing substitution within lambda expressions.
  - `reduction` for reducing lambda expressions by one step.
  - `reduceNTimes` for reducing lambda expressions multiple times.

### Main
- **Expressions:**
  - `lambdaOne` and `lambda` are sample lambda expressions for testing.
- **Tests:**
  - Unit tests for validating the `printLambdaExpress`, `substitution`, `reduction`, and `reduceNTimes` functions.

