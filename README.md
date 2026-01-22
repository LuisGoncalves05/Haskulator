# Haskulator

A small calculator written in Haskell using monadic parsing. It supports arithmetic expressions, variables, and proper operator precedence. The project is a simple demonstration of parsing and evaluation in a functional style.

## Features

- Arithmetic expressions
- Parentheses
- Variable assignment and reuse

## How to Run

### Requirements
- GHC

### Compile
`ghc Calculator.hs`

### Run
`./Calculator`

### Example Usage
```bash
./Calculator
10-5+12
17
(10+5)%(2+7)
6
x=1
1
xx=x*x+1
2
(x+xx+1)*xx
8
```
