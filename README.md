tic-tac-toe.hs
==============

Tic Tac Toe in Haskell


Install
---

Get the [Haskell Platform](http://www.haskell.org/platform/)

Probably the best way on a mac is to use Homebrew:

```bash
brew install haskell-platform
```

To be able to run the tests, you need to install HSpec:

```bash
cabal update
cabal install hspec
```

Running
---

On Unix, type:

```bash
runghc Main.hs
```

If you want it as a standalone executable, you can type:

```bash
ghc Main.hs -o tic
```

The executable will be named `tic`
