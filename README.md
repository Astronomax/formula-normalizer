# formula-normalizer
[![Haskell CI](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml/badge.svg)](https://github.com/SmnTin/simple-type-checker/actions/workflows/haskell.yml)

A simple translator of formulas into normal forms:
*   Negative Normal Form (NNF)
*   Disjunctive Normal Form (DNF)
*   Conjunctive Normal Form (CNF)
## Quick start

In the project root folder run the following to build the project:
```console
$ stack update
$ stack build
```

Then run the normalizer with exactly one argument. For example:
```console
$ stack run -- "M <-> ~(K \/ N)"
NNF: (~M \/ ~K /\ ~N) /\ (K \/ N \/ M)
DNF: ~M /\ K \/ ~M /\ N \/ ~M /\ M \/ (~K /\ ~N /\ K \/ ~K /\ ~N /\ N \/ ~K /\ ~N /\ M)
CNF: (~M \/ ~K) /\ (~M \/ ~N) /\ (K \/ N \/ M)
```
If the number of arguments differs from 1, you'll receive the error message:
```console
$ stack run -- "Rostov" "Moscow" "Yekaterinburg"
InvalidArguments
```
To test the project run the following command:
```console
$ stack test
```

## Syntax

### Expression variables

Variable names can contain only letters of both registers.

Examples of valid variable names: `abcd`, `ABCD`, `AbCd`  

Examples of invalid variable names: `frD^jej`, `sDA'''`, `eFeD@`

### Logical constants

Use the characters `0` and `1` as synonyms of _False_ and _True_.

### Operators

|  Op                  | Priority | Type      | Associativity    | Syntax |
|----------------------|----------|-----------|------------------|--------|
|  Not                 | 5        | Prefix    | -                | `~`    |
|  And                 | 4        | Infix     | Left             | `/\`   |
|  Or                  | 3        | Infix     | Left             | `\/`   |
|  Implication         | 2        | Infix     | Left             | `->`   |
|  Double Implication  | 1        | Infix     | Left             | `<->`  |

It is forbidden to use more than one `~` character in a row. Use brackets `(`, `)` to separate.    

Examples of valid formulas: `~a`, `~(~a)`, `~(~a /\ b)`  

Examples of invalid formulas: `~~a`, `~~~a`, `~~(~a /\ b)`  

If the expression does not match the format, you'll receive the error message:
```console
$ stack run -- "~~a"
ParsingError
```

### Examples
```console
$ stack run -- "~(a /\ b)"
NNF: ~a \/ ~b
DNF: ~a \/ ~b
CNF: ~a \/ ~b
```
```console
$ stack run -- "1 <-> a"
NNF: (~1 \/ a) /\ (~a \/ 1)
DNF: ~1 /\ ~a \/ ~1 /\ 1 \/ (a /\ ~a \/ a /\ 1)
CNF: (~1 \/ a) /\ (~a \/ 1)
```
```console
$ stack run -- "1 -> (~a /\ b /\ c) <-> 0"
NNF: (1 /\ (a \/ ~b \/ ~c) \/ 0) /\ (~0 \/ (~1 \/ ~a /\ b /\ c))
DNF: 1 /\ a /\ ~0 \/ (1 /\ a /\ ~1 \/ 1 /\ a /\ (~a /\ b /\ c)) \/ (1 /\ ~b /\ ~0 \/ (1 /\ ~b /\ ~1 \/ 1 /\ ~b /\ (~a /\ b /\ c))) \/ (1 /\ ~c /\ ~0 \/ (1 /\ ~c /\ ~1 \/ 1 /\ ~c /\ (~a /\ b /\ c))) \/ (0 /\ ~0 \/ (0 /\ ~1 \/ 0 /\ (~a /\ b /\ c)))
CNF: (1 \/ 0) /\ (a \/ ~b \/ ~c \/ 0) /\ ((~0 \/ (~1 \/ ~a)) /\ (~0 \/ (~1 \/ b)) /\ (~0 \/ (~1 \/ c)))
```
