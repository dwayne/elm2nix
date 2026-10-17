json_parser_rev := "5764dec55b7928f42b4d35ba062f50f1e23b8041"
json_codec_rev := "838de8206a5b6192e910ce0dc2fe61e0da1aea12"

# List the available recipes
[private]
default:
    @just --list --unsorted

# Build elm2nix
[positional-arguments]
build *args:
    cabal build "$@"

# Generate the Haddock docs
docs:
    cabal haddock

# Generate the Haddock docs and open them in a browser
docs-open:
    cabal haddock --open

# Generate nix/generated/json-parser.nix
gen-json-parser: (cabal2nix "json-parser" "https://github.com/dwayne/hs-json-parser/archive/" + json_parser_rev + ".tar.gz")

# Generate nix/generated/json-codec.nix
gen-json-codec: (cabal2nix "json-codec" "https://github.com/dwayne/hs-json-codec/archive/" + json_codec_rev + ".tar.gz")

# Generate nix/generated/elm2nix.nix
gen-elm2nix: (cabal2nix "elm2nix" "../..")

# Generate all Haskell derivations
gen: gen-json-parser gen-json-codec gen-elm2nix

[private]
cabal2nix name src:
    #!/usr/bin/env bash
    set -euo pipefail
    cd nix/generated
    out="{{name}}.nix"
    trap 'rm -f "$out.tmp"' EXIT
    cabal2nix --no-haddock --no-check "{{src}}" > "$out.tmp"
    mv "$out.tmp" "$out"

# Static checks (fast, platform-independent)
lint:
    actionlint
    cabal check
    hlint app src tests

# Run the test suite
[positional-arguments]
test *args:
    cabal test "$@"

# Check the flakes
flake-check:
    nix flake check -L
    nix flake check ./example -L

# Remove generated artifacts
clean:
    rm -rf dist-newstyle result example/elm-stuff example/result

alias b := build
alias d := docs
alias o := docs-open
alias g := gen
alias l := lint
alias t := test
