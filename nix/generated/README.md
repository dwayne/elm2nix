# Notes

## How was `elm2nix.nix` generated?

```bash
(cd nix/generated && cabal2nix --no-haddock --no-check ../.. > elm2nix.nix)
```

## How was `json-codec.nix` generated?

```bash
(cd nix/generated && cabal2nix --no-haddock --no-check --revision 838de8206a5b6192e910ce0dc2fe61e0da1aea12 \
  https://github.com/dwayne/hs-json-codec.git > json-codec.nix)
```

## How was `json-parser.nix` generated?

```bash
(cd nix/generated && cabal2nix --no-haddock --no-check --revision 5764dec55b7928f42b4d35ba062f50f1e23b8041 \
  https://github.com/dwayne/hs-json-parser.git > json-parser.nix)
```
