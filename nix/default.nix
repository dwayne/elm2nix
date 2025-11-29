{ brotli
, elmPackages
, fetchzip
, lib
, runCommand
, stdenv
, terser
, uglify-js

, elmVersion ? "0.19.1"
}:
let
  mkElmDerivation =
    { elmLock # Path to elm.lock
    , registryDat # Path to registry.dat

    , doValidateFormat ? false
    , elmFormatInputs ? [ "src" ]

    , doElmReview ? false
    , elmReviewFlags ? []
    , elmReviewElmLock ? throw "elmReviewElmLock is required when doElmReview is true"
    , elmReviewRegistryDat ? throw "elmReviewRegistryDat is required when doElmReview is true"

    , doElmTest ? false
    , elmTestFlags ? []

    , entry ? "src/Main.elm" # :: String | [String]

    , output ? "elm.js" # :: String
    , outputMin ? "${lib.removeSuffix ".js" output}.min.js"

    , extraNativeBuildInputs ? []

    , enableDebugger ? false

    , enableOptimizations ? false
    , optimizeLevel ? 1 # :: 1 | 2 | 3

    , enableMinification ? false
    , useTerser ? false # Use UglifyJS by default

    , enableCompression ? false
    , gzipFlags ? [ "-9" ]
    , brotliFlags ? [ "-Z" ]

    , showStats ? false

    , enableHashedFilenames ? false
    , hashLength ? 8
    , replaceWithHashedFilenames ? true

    , ...
    } @ args:

    assert !(enableDebugger && enableOptimizations)
      || throw "You cannot enable both debugging and optimizations.";

    assert !(enableDebugger && enableMinification)
      || throw "You cannot enable both debugging and minification.";

    assert !(enableDebugger && enableCompression)
      || throw "You cannot enable both debugging and compression.";

    let
      useElmOptimizeLevel2 = enableOptimizations && optimizeLevel >= 2;
      minifier = if useTerser then "terser" else "uglifyjs";
      toCompress = if enableMinification then outputMin else output;

      defaultElmLock = elmLock;
      defaultRegistryDat = registryDat;

      prepareElmHomeScript =
        { elmLock ? defaultElmLock
        , registryDat ? defaultRegistryDat
        , directory ? ".elm"
        }: ''
        echo "Prepare ${directory} and set ELM_HOME=${directory}"
        cp -LR "${dotElmLinks { inherit elmLock registryDat; }}" ${directory}
        chmod -R +w ${directory}
        export ELM_HOME="$PWD/${directory}"
      '';

      dotElmLinks =
        { elmLock ? defaultElmLock
        , registryDat ? defaultRegistryDat
        }:
        runCommand "dot-elm-links" {} ''
          root="$out/${elmVersion}/packages"
          mkdir -p "$root"

          ln -s "${registryDat}" "$root/registry.dat"

          ${symbolicLinksToPackagesScript { inherit elmLock; }}
        '';

      symbolicLinksToPackagesScript =
        { elmLock ? defaultElmLock
        }:
        builtins.foldl'
          (script: { author, package, version, sha256 } @ dep:
            script + ''
              mkdir -p "$root/${author}/${package}"
              ln -s "${fetchElmPackage dep}" "$root/${author}/${package}/${version}"
            ''
          )
          ""
          (lib.importJSON elmLock);
    in
    stdenv.mkDerivation (args // {
      nativeBuildInputs = builtins.concatLists
        [ ([ elmPackages.elm ]
          ++ lib.optional doValidateFormat elmPackages.elm-format
          ++ lib.optional doElmReview elmPackages.elm-review
          ++ lib.optional doElmTest elmPackages.elm-test
          ++ lib.optional useElmOptimizeLevel2 elmPackages.elm-optimize-level-2
          ++ lib.optional enableMinification (if useTerser then terser else uglify-js)
          ++ lib.optional enableCompression brotli)
          extraNativeBuildInputs
        ];

      dontPatch = true;
      dontConfigure = true;

      preBuildPhases = [
        (lib.optionalString doValidateFormat "validateFormatPhase")
        (lib.optionalString doElmReview "elmReviewPhase")
        "prepareElmHomePhase"
        (lib.optionalString doElmTest "elmTestPhase")
      ];

      validateFormatPhase = lib.optionalString doValidateFormat ''
        elm-format ${builtins.concatStringsSep " " elmFormatInputs} --validate
      '';

      elmReviewPhase = lib.optionalString doElmReview ''
        if [ -d review ]; then
          ${prepareElmHomeScript { elmLock = elmReviewElmLock; registryDat = elmReviewRegistryDat; directory = ".elm-review"; }}

          echo elm-review ${builtins.concatStringsSep " " elmReviewFlags} --offline "is disabled since it hasn't been working as expected"
        else
          echo "Skipping elm-review since no review/ directory was found"
        fi
      '';

      prepareElmHomePhase = prepareElmHomeScript {};

      elmTestPhase = lib.optionalString doElmTest ''
        if [ -d tests ]; then
          elm-test ${builtins.concatStringsSep " " elmTestFlags}
        else
          echo "Skipping elm-test since no tests/ directory was found"
        fi
      '';

      buildPhase =
        let
          buildScript =
            if useElmOptimizeLevel2 then
              let
                inputFiles =
                  if builtins.isList entry then
                    builtins.warn "elm-optimize-level-2 accepts multiple input files but only processes the first" entry
                  else
                    [ entry ];
              in
              ''
              elm-optimize-level-2 \
                ${builtins.concatStringsSep " " inputFiles} \
                ${lib.optionalString (optimizeLevel >= 3) "--optimize-speed"} \
                --output ".build/${output}"
              ''
            else
              ''
              elm make \
                ${builtins.concatStringsSep " " (if builtins.isList entry then entry else [ entry ])} \
                ${lib.optionalString enableDebugger "--debug"} \
                ${lib.optionalString (enableOptimizations && optimizeLevel == 1) "--optimize"} \
                --output ".build/${output}"
              '';
        in
        ''
        runHook preBuild

        ${buildScript}

        runHook postBuild
        '';

      installPhase = ''
        runHook preInstall

        cp -R .build "$out"

        runHook postInstall
      '';

      #
      # Learn more: https://guide.elm-lang.org/optimization/asset_size
      #

      preFixupPhases =
        (lib.optional enableMinification "minificationPhase")
        ++ (lib.optional enableCompression "compressionPhase")
        ++ (lib.optional showStats "showStatsPhase")
        ++ (lib.optional enableHashedFilenames "contentHashingPhase")
        ;

      minificationPhase = lib.optional enableMinification ''
        ${minifier} "$out/${output}" \
          --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
          | ${minifier} --mangle --output "$out/${outputMin}"
      '';

      compressionPhase = lib.optional enableCompression ''
        gzip ${builtins.concatStringsSep " " gzipFlags} -c "$out/${toCompress}" > "$out/${toCompress}.gz"
        brotli ${builtins.concatStringsSep " " brotliFlags} -c "$out/${toCompress}" > "$out/${toCompress}.br"
      '';

      showStatsPhase = lib.optionalString showStats ''
        js="${output}"
        js_size=$(stat -c%s $out/$js)
        echo "Compiled size: $js_size bytes ($js)"

        ${lib.optionalString enableMinification ''
          min="${outputMin}"
          min_size=$(stat -c%s $out/$min)
          min_pct=$(( 100 * min_size / js_size ))
          echo "Minified size: $min_size bytes ($min) (''${min_pct}% of compiled)"
        ''}

        ${lib.optionalString enableCompression ''
          gz="${toCompress}.gz"
          gz_size=$(stat -c%s $out/$gz)
          gz_pct=$(( 100 * gz_size / js_size ))
          br="${toCompress}.br"
          br_size=$(stat -c%s $out/$br)
          br_pct=$(( 100 * br_size / js_size ))
          echo "Gzipped size: $gz_size bytes ($gz) (''${gz_pct}% of compiled)"
          echo "Brotlied size: $br_size bytes ($br) (''${br_pct}% of compiled)"
        ''}
      '';

      contentHashingPhase = lib.optionalString enableHashedFilenames (
        assert (hashLength >= 1 && hashLength <= 64)
          || throw "hashLength must be between 1 and 64 inclusive: ${toString hashLength}";

        ''
        manifest="$(mktemp)"

        echo "{" > "$manifest"
        first_entry=1

        for file in "$out"/*; do
          hash=$(sha256sum "$file" | cut -c 1-${toString hashLength})

          filename="''${file##*/}"
          base="''${filename%%.*}"
          ext="''${filename#*.}"
          hashedFilename="$base.$hash.$ext"

          ${if replaceWithHashedFilenames then "mv" else "cp"} "$file" "$out/$hashedFilename"
          echo ${if replaceWithHashedFilenames then "Moved" else "Copied"} "$filename" "───>" "$hashedFilename"

          if [ $first_entry -eq 0 ]; then
            echo "," >> "$manifest"
          fi
          first_entry=0

          printf '    "%s": "%s"' "$filename" "$hashedFilename" >> "$manifest"
        done

        echo "" >> "$manifest"
        echo "}" >> "$manifest"
        cp "$manifest" "$out/manifest.json"
        '');

      passthru = {
        inherit prepareElmHomeScript dotElmLinks symbolicLinksToPackagesScript;
      };
    });

  fetchElmPackage = { author, package, version, sha256 }:
    fetchzip {
      name = "${author}-${package}-${version}";
      url = "https://github.com/${author}/${package}/archive/${version}.tar.gz";
      meta.homepage = "https://github.com/${author}/${package}";
      hash = builtins.convertHash {
        hash = sha256;
        hashAlgo = "sha256";
        toHashFormat = "sri";
      };
    };
in
{
  mkElmDerivation = lib.makeOverridable mkElmDerivation;
  inherit fetchElmPackage;
}
