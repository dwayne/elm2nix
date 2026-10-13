{ fetchurl
, lib
, makeWrapper
, nodejs
, stdenv
}:

let
  # The hashes were calculated based on the SHA-256 digests
  # found on the release page.
  #
  # See https://github.com/elm/compiler/releases/tag/0.19.3.
  #
  # nix hash convert --hash-algo sha256 --to sri sha256:...
  sources = {
    x86_64-linux = {
      suffix = "linux-x64";
      hash = "sha256-0RcCsxV37YtyC52BxSyKGaUrcrH8+t7w/K2S8heYxYo=";
    };

    aarch64-linux = {
      suffix = "linux-arm";
      hash = "sha256-TSLE0Q+J5ILTVUt69PZSf4oF5NgmSKB7I/HyEUlbXSQ=";
    };

    x86_64-darwin = {
      suffix = "mac-x64";
      hash = "sha256-SpxVeOU8T+tVuRyO6aD0bamRNBTGMD0cVa3Q06vNmSE=";
    };

    aarch64-darwin = {
      suffix = "mac-arm";
      hash = "sha256-pCbkfRTLoFiKqBR1rcMroku/BFIJ0MVg0KiaXE88NNA=";
    };
  };

  system = stdenv.hostPlatform.system;

  source = sources.${system} or (throw "elm: unsupported system ${system}");
in
stdenv.mkDerivation (finalAttrs: {
  pname = "elm";
  version = "0.19.3";

  src = fetchurl {
    url = "https://github.com/elm/compiler/releases/download/${finalAttrs.version}/elm-${finalAttrs.version}-${source.suffix}.gz";
    inherit (source) hash;
  };

  nativeBuildInputs = [
    makeWrapper
  ];

  unpackPhase = ''
    runHook preUnpack

    gzip -dc $src > elm

    runHook postUnpack
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 elm $out/bin/elm

    runHook postInstall
  '';

  postInstall = ''
    wrapProgram "$out/bin/elm" \
      --prefix PATH ':' ${lib.makeBinPath [ nodejs ]}
  '';

  meta = {
    mainProgram = finalAttrs.pname;
    platforms = builtins.attrNames sources;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
