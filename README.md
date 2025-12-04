# elm2nix

## Documentation

- [GHC Documentation](https://downloads.haskell.org/ghc/9.10.2/docs/)

## `elm-review` fails

`nix build .#reviewedExample -L`

```
example> Running phase: unpackPhase
example> unpacking source archive /nix/store/l77af4k6mm6nd5i2az3jjw1dq7i5jhyr-example
example> source root is example
example> Running phase: updateAutotoolsGnuConfigScriptsPhase
example> Running phase: elmFormatPhase
example> []
example> Running phase: elmReviewPhase
example> Prepare .elm-review and set ELM_HOME=.elm-review
example> I could not fetch all the data I need about your project’s dependencies. Please
example> connect to the Internet so I can download and cache the data for future uses.
example> I will try to review the project anyway, but you might get unexpected results…
example>
example> -- ELM-REVIEW ERROR ---------------------------------------------- elm.json:9:14
example>
example> (fix) NoUnused.Dependencies: Unused dependency `elm/browser`
example>
example>  8|         "direct": {
example>  9|             "elm/browser": "1.0.2",
example>                  ^^^^^^^^^^^
example> 10|             "elm/core": "1.0.5",
example>
example> To remove it, I recommend running the following command:
example>
example>     elm-json uninstall elm/browser
example>
example> Errors marked with (fix) can be fixed automatically using `elm-review --fix`.
example>
example> I found 1 error in 1 file.
```

However, when I run `elm-review` from the development environment it says there are no errors.
