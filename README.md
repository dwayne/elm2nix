# elm2nix

A rewrite of [`cachix/elm2nix`](https://github.com/cachix/elm2nix) with a few improvements.

## Usage

### Overview

In the folder containing your Elm application's `elm.json` you use:

- `elm2nix lock` to generate a lock file called `elm.lock` by default
- `elm2nix registry generate` to generate `registry.dat`

These generated files are used by a builder called `buildElmApplication` to build various derivations of your Elm application.

### Details

1. Add `dwayne/elm2nix` as an input to your flake.

```nix
inputs.elm2nix.url = "github:dwayne/elm2nix";
```

2. Add it's default package to your development shell.

```nix
outputs = { self, nixpkgs, flake-utils, elm2nix }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            elm2nix.packages.${system}.default
            ...
          ];

          ...
        };

        ...
      }
    );
```

3. Use the `elm2nix` program to generate the `elm.lock` and `registry.dat` files from your Elm application's `elm.json`.

```bash
nix develop
elm2nix lock              # generates elm.lock
elm2nix registry generate # generates registry.dat
```

See `elm2nix --help` for more details.

4. Use `buildElmApplication` to build your Elm application.

```nix
outputs = { self, nixpkgs, flake-utils, elm2nix }:
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (elm2nix.lib.elm2nix pkgs) buildElmApplication;

        myApp = buildElmApplication {
          name = "my-app";
          src = ./.;
          elmLock = ./elm.lock;
          registryDat = ./registry.dat;
        };
      in
      {
        packages.myApp = myApp;

        ...
      }
    );
```

5. Build your Elm application.

```bash
nix build .#myApp
```

This will generate a `result/` directory containing your compiled application in `elm.js`.

6. That's it.

You can view a full working example in the [`example/`](./example) directory.

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
