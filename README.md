# Lambda Calculus Expressions and Reductions

Amelia Hoyos y Valentina Vasquez. 

## System Information
- **Operating System**: Microsoft Windows Version 23H2
- **GHC Version**: 9.8.2
- **HUnit Version**: 1.6.2.0
- **Cabal Version**: 3.10.3.0

## Overview
This repository contains Haskell code for representing and manipulating lambda calculus expressions. It includes functionality for printing lambda expressions, performing substitutions, reducing expressions, reducing expressions n times, and checking if the expression is normalized. Additionally, the repository includes unit tests for validating the correctness of these functions.

## Modules
### LC
- **Data Types:**
  - `LambdaExpress` for representing lambda calculus expressions (variables, abstractions, and applications).
- **Functions:**
  - `printLambdaExpress` for converting lambda expressions to their string representation.
  - `substitution` for performing substitution within lambda expressions.
  - `reduction` for reducing lambda expressions by one step.
  - `tnf` for reducing lambda expressions multiple times.
  - `isNormalForm`: To check if lambda expression is in normal form
  - `isNormalized`: To send a message if the expression is not in normal form
  - `printOriginalAndResult`: To print the original expression, the resources available n and the final expression when n reductions are applied.

### Main
### Expressions
  - `lambdaIdentity`, `lambdaBinaryFunction`, and `lambdaNoNormal` are sample lambda expressions for testing.

1. `lambdaIdentity`: An identity function applied to the variable "Otter."
2. `lambdaBinaryFunction`: A lambda expresssion representing the application of a binary function. In this case the `+` applied to `3` and ``4`.
3. `lambdaNoNormal`: A non-normalizing lambda expression.

#### Normalization Testing

The `testNormalization` function conducts several tests on the normalization process:

1. Tests on `lambdaIdentity` for normalization within different step counts.
2. Tests on `lambdaBinaryFunction` for normalization within different step counts.
3. Tests on `lambdaNoNormal` to confirm its non-normalization.

#### Test Functions

- `checkNormalizationList`: A function to check normalization for a list of terms at specified step counts.

#### Main Function

The `main` function executes the normalization tests and checks for each lambda expression using various step counts. Additionally, it runs the `testNormalization` suite using `runTestTT` from the `Test.HUnit` module.

### How to Use

To use this module:

1. Define lambda expressions.
2. Use `checkNormalizationList` to check normalization for a list of terms at specified step counts.
3. Execute the `main` function to perform normalization tests and checks.

### Dependencies

- `LC`: The module `LC` containing lambda calculus evaluation functions.
- `Test.HUnit`: The testing framework used for unit tests.

### Resources used
- Beta Normal Form. Wikipedia, https://en.wikipedia.org/wiki/Beta_normal_form.
- Beta Normalization. Dhall-lang GitHub Repository, https://github.com/dhall-lang/dhall-lang/blob/master/standard/beta-normalization.md.
- Build Dependency in Library or Executable Section of Cabal File. Stack Overflow, https://stackoverflow.com/questions/62286703/build-dependency-in-library-or-executable-section-of-cabal-file.
- Cabal User Guide. Cabal Documentation, https://cabal.readthedocs.io/en/3.4/getting-started.html.
- Lambda Calculus Reduction Steps. Stack Overflow, https://stackoverflow.com/questions/34140819/lambda-calculus-reduction-steps.
- Lambda Calculus: No Lambda Normal Form? Computer Science Stack Exchange, https://cs.stackexchange.com/questions/104710/no-lambda-normal-form.
- Lambda Calculus PDF. University of Birmingham, https://www.cs.bham.ac.uk/~axj/pub/papers/lambda-calculus.pdf.
- Lambda Calculus in Haskell. M. Cordier, https://mcordier.xyz/blog/lbd_hskl/.
- OpenAI. “GPT Models.” OpenAI, 2022, https://openai.com/gpt.
- Types. Haskell Wiki, https://wiki.haskell.org/Type.
- Var, Bound, and Free Variables. Stanford Encyclopedia of Philosophy, https://plato.stanford.edu/entries/lambda-calculus/#VarBouFre.
- YouTube Video: Cálculo Lambda. https://www.youtube.com/watch?v=i1zYBLdlxfc.

