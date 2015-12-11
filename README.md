# Haphviz
Graphviz code generation with Haskell

[![Build Status](https://travis-ci.org/NorfairKing/haphviz.svg?branch=master)](https://travis-ci.org/NorfairKing/haphviz)

## Examples

There are some examples in the `examples` directory.

Running them with `runhaskell examples/example.hs` will get you the graphviz code.
If you have `xdot`, then you can run `runhaskell examples/example.hs | xdot` to see the graph visualized.

## Git hooks

Before making any contributions, please install the git hooks.
```
spark deploy hooks.sus
```
