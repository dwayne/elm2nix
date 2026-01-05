# Notes

## How was `elm2nix.nix` generated?

```bash
(cd nix/generated && cabal2nix --no-haddock --no-check ../.. > elm2nix.nix)
```
