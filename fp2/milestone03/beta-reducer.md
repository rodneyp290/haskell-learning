# Milestone 03 - Lambda Calculus Beta Reducer

**Status:** All tests passing

## Test Cases 
1. (λx.x) y => y
2. (λf.λa.f a) (λx.x x) y = (y y)
3. (λb.λt.λe.(b t) e)(λx.λy.x) x y => x
4. (λx.λy.x y) (y w) => λz.(y w) z (free variables)
5. (λx.λx.x) x y => y (shadowing)
6. ((λx.x) (λx.x)) x => x

## Lambda Expression

```
LExpr = Lambda Char LExpr |
        App LExpr LExpr 
        Free Char
```

