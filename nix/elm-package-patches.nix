#
# The values of all the keys in this attribute set corresponds to
# the arguments that can be passed to mkPatch.
#
# As a result, these values can also be passed directly to
# the elmPackagePatches option of the buildElmApplication function.
#
{
  lydellBrowser = {
    fromOwner = "lydell";
    toOwner = "elm";
    repo = "browser";
    version = "1.0.2";
    rev = "f5de544c8033d934285501f78f09e2eaf0171d55";
    hash = "sha256-29axLnzXcLDeKG+CBX49pjt2ZcYVdVg04XVnfAfImvI=";
  };

  lydellHtml = {
    fromOwner = "lydell";
    toOwner = "elm";
    repo = "html";
    version = "1.0.1";
    rev = "b35c476a69f0ba9bf8282d8c15df65e63aefea8f";
    hash = "sha256-xyL/AvKdsxTl4RgfBCdTuWndM55eNM6whPD3YqptcKM=";
  };

  lydellVirtualDom = {
    fromOwner = "lydell";
    toOwner = "elm";
    repo = "virtual-dom";
    version = "1.0.5";
    rev = "e1fae6aabd65539db2c94a98220a45cfc624b633";
    hash = "sha256-XpbRMCpIx151eHHoph7wkGYhtDp5bTBwUOefiWKItOc=";
  };

  omnibsElmCss = {
    fromOwner = "omnibs";
    toOwner = "rtfeldman";
    repo = "elm-css";
    version = "18.0.0";
    rev = "e54998ce73b64c374b1457d5734c85d3f5b909fb";
    hash = "sha256-rmil+7lAKUm7Fm0MCba23xyCA0CWrDb1ej5gPeXS2oU=";
  };
}
