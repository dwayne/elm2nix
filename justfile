set positional-arguments

# List the available recipes
default:
    @just --list

# Build the project
build *args:
    cabal build "$@"

# Lint src/ and tests/ with HLint
lint *args:
    hlint src tests "$@"

# Run the test suite
test *args:
    cabal test "$@"

# Generate the Haddock docs
docs:
    cabal haddock

# Generate the Haddock docs and open them in a browser
docs-open:
    cabal haddock --open

# Run every check
check: test
    cabal check
    nix flake check -L
    cd example && nix flake check -L
    actionlint

alias b := build
alias l := lint
alias t := test
alias d := docs
alias o := docs-open
alias c := check
