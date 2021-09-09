# LambdaCalculator

Definition of lambda terms exists in LambdaDefinition module.

For enable functions from certain module, run:
```bash
ghci <module>.hs
```

LambdaCalculator contains all necessary functions for beta-reduction and beta-equality.

LambdaParser can serialize and deserialize lambda expressions. For example:
```bash
GHCi> show $ Lam "x" (Var "x" :@ Var "y")
"\\x -> x y"
GHCi> cY = let {x = Var "x"; f = Var "f"; fxx = Lam "x" $ f :@ (x :@ x)} in Lam "f" $ fxx :@ fxx
GHCi> show cY
"\\f -> (\\x -> f (x x)) (\\x -> f (x x))"
GHCi> cY
\f -> (\x -> f (x x)) (\x -> f (x x))
```
