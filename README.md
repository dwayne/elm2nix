# elm2nix

A rewrite of [`cachix/elm2nix`](https://github.com/cachix/elm2nix) with a few changes and improvements.

These are some of the notable differences:

1. It uses a JSON formatted lock file that has support for multiple versions of the same package.
2. You can display any `registry.dat` as JSON using `elm2nix registry view`.
3. There is a `buildElmApplication` build helper that allows you to build an Elm application in a variety of ways by setting options. For e.g.
    - Turn on the time-travelling debugger
    - Fail the build if your Elm source code is improperly formatted
    - Fail the build if your Elm tests fail
    - Turn on optimizations
    - Use [`elm-optimize-level-2`](https://github.com/mdgriffith/elm-optimize-level-2) instead of `elm make --optimize`
    - Enable minification with [UglifyJS](https://github.com/mishoo/UglifyJS) or [Terser](https://terser.org/)
    - Enable compression with [gzip](https://www.gnu.org/software/gzip/) and [brotli](https://github.com/google/brotli)
    - [Show a report](https://guide.elm-lang.org/optimization/asset_size#scripts) about the changes in your file size due to minification and compression
    - Enable content hashing for cache busting purposes
    - Or completely customize portions of the build to your liking if you know how [`stdenv.mkDerivation`](https://nixos.org/manual/nixpkgs/stable/#chap-stdenv) works

## Usage

### Overview

In the folder containing your Elm application's `elm.json` you use:

- `elm2nix lock` to generate an `elm.lock` lock file
- `elm2nix registry generate` to generate a `registry.dat` file

These generated files are then used by a build helper, called `buildElmApplication`, to build your Elm application.

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

## About

I was looking for a practical and interesting project to work on that might be useful for the Elm community and at the same time extend my skills within functional programming. When I researched [`cachix/elm2nix`](https://github.com/cachix/elm2nix) it seemed to fit the bill because it combined 3 technologies I loved, Elm, Haskell, and Nix to produce a useful tool for Elm developers. At the time, I knew how to use each of the technologies to varying degrees but I didn't have a deep understanding of how `cachix/elm2nix` worked and why it needed to work that way. As a result, it seemed like a great project to satisfy my goals. Fast forward 2 months and I'm happy I took the plunge. I learned so much about Elm, Haskell, and Nix that I didn't know before and I was able to find little ways to usefully improve upon the project.

## References

A grossly incomplete list of resources that helped me while working on the project.

- [elm2nix 0.1](https://blog.hercules-ci.com/elm/2019/01/03/elm2nix-0.1/)
  - [NixOS/nixpkgs - elm2nix issue 20601](https://github.com/NixOS/nixpkgs/issues/20601)
- [`cachix/elm2nix`](https://github.com/cachix/elm2nix)
- [`jeslie0/mkElmDerivation`](https://github.com/jeslie0/mkElmDerivation)
  - [Default to terser instead of uglifyjs](https://github.com/jeslie0/mkElmDerivation/issues/13)
  - [Fork: `r-k-b/mkElmDerivation`](https://github.com/r-k-b/mkElmDerivation)
- [`haskellPackages.mkDerivation`](https://nixos.org/manual/nixpkgs/stable/#haskell-mkderivation)
  - [`pkgs/development/haskell-modules/generic-builder.nix`](https://github.com/NixOS/nixpkgs/blob/b165db247068f3cd0a1d4df0189f8824f59b8279/pkgs/development/haskell-modules/generic-builder.nix)
- [`buildNpmPackage`](https://nixos.org/manual/nixpkgs/stable/#javascript-buildNpmPackage)
  - [`pkgs/build-support/node/build-npm-package/default.nix`](https://github.com/NixOS/nixpkgs/blob/b165db247068f3cd0a1d4df0189f8824f59b8279/pkgs/build-support/node/build-npm-package/default.nix)
- [`lib.customisation.extendMkDerivation`](https://nixos.org/manual/nixpkgs/stable/#function-library-lib.customisation.extendMkDerivation)
- [Caching Elm dependencies on GitHub Actions](https://brianvanburken.nl/caching-elm-dependencies-on-github-actions/)
- [What I've learned about minifying Elm code](https://discourse.elm-lang.org/t/what-i-ve-learned-about-minifying-elm-code/7632)
