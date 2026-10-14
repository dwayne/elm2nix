#
# A convenient collection of patches based on lydell/elm-safe-virtual-dom.
#
let
  patches = import ./elm-package-patches.nix;
in
rec {
  default = elmHtml;

  elmHtml = [
    patches.lydellBrowser
    patches.lydellHtml
    patches.lydellVirtualDom
  ];

  elmCss = [
    patches.lydellBrowser
    patches.omnibsElmCss
    patches.lydellVirtualDom
  ];
}
