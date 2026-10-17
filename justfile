set positional-arguments

# List the available recipes
[private]
default:
    @just --list --unsorted

# Build elm2nix
build *args:
    cabal build "$@"

# Generate the Haddock docs
docs:
    cabal haddock

# Generate the Haddock docs and open them in a browser
docs-open:
    cabal haddock --open

# Static checks (fast, platform-independent)
lint:
    actionlint
    cabal check
    hlint app src tests

# Run the test suite
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
alias l := lint
alias t := test
