{ elm-json }:

elm-json.overrideAttrs (old: {
  patches = (old.patches or []) ++ [
    ./elm-json-0.19.3.patch
  ];
})
