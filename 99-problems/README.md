# Ninety-Nine Haskell Problems

## What is this?

This is an adaptation of [Ninety-Nine Lisp Problems](http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html).

## How to install?

First of all, you need a [GitHub account](https://github.com/join) so you can [fork this repo](https://github.com/mkovacs/99-haskell/fork). If you don't feel like signing up, you can also just clone this repo, but then you won't be able to push your changes.

You'll also need [git](http://git-scm.com/) for getting the code, as well as [GHC & Cabal](https://www.haskell.org/platform/) to build and test your solutions.

Clone the repo like this:
```
git clone <uri-to-repo-on-github>
```

To install the depencencies of the project, execute:
```
cabal install --dependencies-only
```

## Now what?

Your task is to replace the ```undefined``` values such that the tests pass.

Here's how to execute the tests for the first ten problems:
```
cabal test problems-01-10
```

The interactive shell can also be very helpful:
```
cabal repl problems-01-10
```
