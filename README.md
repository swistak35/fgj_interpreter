Important Note
=========

This was project for seminar about foundations of object-oriented languages.
I don't intend to support this project in future, it's for only read-only and educational purposes.


FJ, FGJ Tools
=========

Files
----
  * FeatherweightJava.hs - FJ typechecker, evaluator and example from lecture
  * main_fgj.hs - main file, type erasure for FGJ and examples of pair and list
  * Utils.hs - few simple auxiliary functions
  * README.md - this file

Installation
----
You will need haskell, I was using GHC 7.8.2.
No external libraries are required.
Compilation:
```sh
ghc --make main_fgj.hs
```
Run:
```sh
./main_fgj list  # (default)
```
or 
```sh
./main_fgj pair
```

Semantics
----
  * env - mapping from object names to objects
  * objects is a structure: { class (String), array of type_f (String), array of f (Object)} (we can also think about Objects like tuples)
  * mtype(C,m) is returning type of a function, based on global, immutable, Klass Table

Field Call:
```
            env => e -> o
------------------------------------------
  env => e.f[i] -> (o.type_f[i], o.f[i])
```

Object:
```
            env(x) = o
------------------------------------------
        env => x -> (o.class, o)
```

New Object:
```
                     forall.i. env => e[i] -> (t[i], o[i])
       o = { class = c, forall.i. f[i] = t[i], forall.i. type_f[i] = o[i]}
------------------------------------------------------------------------------------
                         env => new C(e[0], e[1], ...) -> (C, o)
```

Type Cast:
```
             env => e -> (C,o)
------------------------------------------
             env => (D)e -> (D,o)
```

Method Call:
```
    env => f[i] -> (t[i], o[i])
    env, o[i] => mbody(e.class, m) -> (R, o')
    mtype(e.class, m) = A[] -> R
------------------------------------------
  env => e.m(f[0], f[1], ...) -> (R, o')
```

