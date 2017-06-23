# Chapter 1

## Exercise Answers

### Intermission: Equivalence Exercises

1. b
2. c
3. b

```
Workings λx.λy.λz.zx a b  
y.λz.za b  
   λz.za
  c)   λm.λn.λp.mn a b
   λn.λp.an b
   λp.ab           -- not alpha equivalent of λz.za
  b)   λt.λo.λs.st a b --already we can see it is the same format so alpha equivalence
   λo.λs.sa b
   λs.sa           -- alpha equivalent of λz.za
```

### Chapter Exercises

#### Combinators
1. Yes
2. no because z is free
3. Yes
4. Yes
5. no because z is free

#### Normal form or diverge?
1. Is in normal form but could diverge if applied to self
2. Can be reduced to λy.yy (beta normal form) -- Incorrect "reduces" to the alpha equivalent (λy.yy)(λy.yy) which diverges
3. Can be reduced to zzz (beta normal form)

#### Beta Reduce

1. ```
   (λabc.cba)zz(λwv.w)
   [a:=z,b:=z,c:=(λwv.w)]cba -- beta reduction
   (λwv.w)zz
   [w:=z,v:=z]w -- beta reduction
   z```

2. ```
  (λx.λy.xyy)(λa.a)b
   ([x:=(λa.a)]λy.xyy)b -- beta reduction
   (λy.(λa.a)yy)b
   (λy.([a:=y]a)y)b)      OR (\[y:=b](λa.a)yy) -- beta reduction
   (λy.(y)y)b -> (λy.yy)b OR (λa.a)bb
   ([y:=b]yy)             OR ([a:=b]a)b -- beta reduction
   bb                     OR (b)b -> bb
```
3. ```
   (λy.y)(λx.xx)(λz.zq)
   ([y=(λx.xx)y)(λz.zq) -- beta reduction
   (λx.xx)(λz.zq)
   ([x:=λz.zq]xx) -- beta reduction
   (λz.zq)(λz.zq)
   [z:=(λz.zq)]zq]  --alpha equivalence may make this more readable
   (λz.zq)q
   ([z:=q]zq) -- beta reduction
   qq
```
4. ```
(λz.z)(λz.zz)(λz.zy) -- because of alpha equivalence Exercise 3
   (λz.z)([z:=x]λz.zz)([z:=w]λz.zy) -- applying alpha equivalence
   (λz.z)(λx.xx)(λw.wy)
   ([z:=(λx.xx)]z)(λw.wy) -- beta reduction
   (λx.xx)(λw.wy)
   ([x:=(λw.wy)]xx) -- beta reduction
   (λw.wy)(λw.wy)
   (λw.wy)([w:=v]λw.wy) -- beta reduction
   (λw.wy)(λv.vy)
   ([w:=(λv.vy)]wy) -- beta reduction
   (λv.vy)y
   ([v:=y]vy)y -- beta reduction
   yy
```
5. ```
   (λx.λy.xyy)(λy.y)y
   (λx.[y:=z]λy.xyy)([y:=w]λy.y)y -- apply alpha equivalence to y functions to avoid confusion
   (λx.λz.xzz)(λw.w)y
   ([x:=(λw.w)]λz.xzz)y -- beta reduction
   (λz.(λw.w)zz)y
   (\[z:=y](λw.w)zz) -- beta reduction
   (λw.w)yy
   ([w:=y]w)y -- beta reduction
   yy
```
6. ```
   (λa.aa)(λb.ba)c
   ([a:=d]λa.aa)(λb.ba)c -- apply alpha equivalence to a function to avoid confusion
   (λd.dd)(λb.ba)c
   ([d:=(λb.ba)]dd)c -- beta reduction
   (λb.ba)(λb.ba)c
   (λb.ba)([b:=e]λb.ba)c -- apply alpha equivalence to 2nd b function to avoid confusion
   (λb.ba)(λe.ea)c
   ([b:=(λe.ea)]ba)c -- beta reduction
   (λe.ea)a(c)
   ([e:=a]ea)a(c) -- beta reduction
   aac
```
7. ```
   (λxyz.xz(yz))(λx.z)(λx.a)
   (λx.λy.λz.xz(yz))(λx.z)(λx.a) -- de-currying
   (λx.λy.([z:=u]λz.xz(yz)))([x:=w]λx.z)([x:=v]λx.a) -- applying alpha equivalence to avoid confusion later
   (λx.λy.λu.xu(yu))(λw.z)(λv.a)
   ([x:=(λw.z)]λy.λu.xu(yu))(λv.a) -- beta reduction
   (λy.λu.(λw.z)u(yu))(λv.a)
   ([y:=(λv.a)]λu.(λw.z)u(yu)) -- beta reduction
   (λu.(λw.z)u((λv.a)u))
   (λu.(λw.z)u((λv.a)u))  -- possibly should stop BNF?
   (λu.([w:=u]z)(([v:=u]a)))  -- or some extra beta reductions
   (λu.(z)((a)))
   λu.za
```
