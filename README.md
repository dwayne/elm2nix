# elm2nix

## Draft

- Explain at a high level what the repository is about
- Show how to use everything that's available
- How to enter the developer shell?
  - `nix develop`
  - What does it give you access to?
- What does the repository contain?
  - The Haskell source code for the `elm2nix` executable
    - How to build it?
    - How to test it?
  - The Nix source code for the `buildElmApplication` builder
    - How to use it?
  - An example Elm application that uses `buildElmApplication` and friends to showcase various examples

Haskell specific stuff:

```
- app
- src
- test
- elm2nix.cabal
```

`elm2nix.nix` is generated with `cabal2nix`. Maybe I should put it in the `nix/generated` directory. It should not be edited manually.

TODO:

- Maybe I should write a Makefile?
  - For both the Haskell package and the example Elm application
- Maybe I should add convenience scripts in `bin` like I typically do
  - `check`
    - `hlint` could be optional
  - `clean`
  - `format`
  - `test`
- Add a README to `example/`
- Add a README to `nix/`
- Add a README to `test/`

## Notes

### How was `elm2nix.nix` generated?

```bash
(cd nix/generated && cabal2nix --no-haddock --no-check ../..)
```

## Documentation

- [GHC Documentation](https://downloads.haskell.org/ghc/9.10.2/docs/)
- Cabal User Guide
- Documentation for all the Haskell libraries I used
- Documentation for Nix and Nixpkgs
