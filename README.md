# `tilec` — the tile programming language (and eventually, its compiler)

tile is a dependently-typed programming language, but this package is mostly an experiment in language and compiler engineering.

I have some aspirations to quantitative type theory, higher-order unification, responsive compilation, levitation, and whole-program optimization, but for now I’m just trying to find my footing; don’t expect to be able to write programs in this any time soon.


# Language notes

There is no syntax or parser right now. tile programs can only be defined as Haskell expressions.

tile is very small: variables, lambda abstractions & applications, one base type (`Type`), and dependent function types. While dependently-typed, tile is not intended to be a consistent logic, in part because I don’t know how to write a termination checker yet. Once I learn about that, I might reconsider, as long as I haven’t painted myself completely into a corner w.r.t. its needs.

tile does not have datatypes. The intention is to levitate definitions using Böhm-Berarducci/Mendler encodings instead. I intend there to eventually be be syntax sugar for defining datatypes & pattern matching using these methods.

Likewise, tile does not have (much of) a metalanguage. The intention is for modules and such to be modelled as (possibly parametric) records (which tile also does not have), themselves encoded as described above.

Various parts of this plan might be poorly-thought-out, intractable, what-have-you; that’s ok! The goal is to see what I learn from the process, not to have all the answers already.


# Development

Make sure you have a recent enough `ghc` and `cabal`; I’m currently developing with `ghc` 8.8 & `cabal` 3.0, and I’m not testing against older versions. On macOS, I recommend `ghcup`.

I do just about everything via `ghci`, which can be conveniently initialized and launched as follows:

```
cabal build # make sure dependencies are known & installed
script/repl # actually launch the repl
```

`ghcide` integration is also provided, and I edit in VS Code configured to use it.


# Architecture

Broadly, `tilec` is designed to use (untyped) tagless final encodings of DSLs wherever feasible. That means typeclasses for bits of syntax, with instances providing algebras, rather than datatypes & pattern matching.
