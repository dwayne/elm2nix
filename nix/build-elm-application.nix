{ brotli
, elmPackages
, lib
, stdenv
, terser
, uglify-js

, generateRegistryDat
, prepareElmHomeScript
}:

lib.extendMkDerivation {
  constructDrv = stdenv.mkDerivation;

  extendDrvArgs =
    finalAttrs:
    { elmLock # Path to elm.lock

    , doElmFormat ? false # Whether or not to check if a given set of Elm files are formatted
    , elmFormatSourceFiles ? [ "src" ] # A list of Elm files or directories containing Elm files

    , doElmReview ? false
    , elmReviewFlags ? []

    , doElmTest ? false
    , elmTestFlags ? []

    , entry ? "src/Main.elm" # :: String | [String]

    , output ? "elm.js" # :: String
    , outputMin ? "${lib.removeSuffix ".js" output}.min.js"

    , extraNativeBuildInputs ? []

    , enableDebugger ? false

    , enableOptimizations ? false
    , optimizeLevel ? 1 # :: 1 | 2 | 3

    , doMinification ? false
    , useTerser ? false # Use UglifyJS by default

    , doCompression ? false
    , gzipFlags ? [ "-9" ]
    , brotliFlags ? [ "-Z" ]

    , doReporting ? false

    , doContentHashing ? false
    , hashLength ? 8
    , keepFilesWithNoHashInFilenames ? false

    , ...
    } @ args:
      assert !(enableDebugger && enableOptimizations)
        || throw "You cannot enable both debugging and optimizations.";

      assert !(enableDebugger && doMinification)
        || throw "You cannot enable both debugging and minification.";

      assert !(enableDebugger && doCompression)
        || throw "You cannot enable both debugging and compression.";

      let
        registryDat = generateRegistryDat { inherit elmLock; };
        useElmOptimizeLevel2 = enableOptimizations && optimizeLevel >= 2;
        minifier = if useTerser then "terser" else "uglifyjs";
        toCompress = if doMinification then outputMin else output;
      in
      {
        nativeBuildInputs = builtins.concatLists
          [ ([ elmPackages.elm ]
            ++ lib.optional doElmFormat elmPackages.elm-format
            ++ lib.optional doElmReview elmPackages.elm-review
            ++ lib.optional doElmTest elmPackages.elm-test
            ++ lib.optional useElmOptimizeLevel2 elmPackages.elm-optimize-level-2
            ++ lib.optional doMinification (if useTerser then terser else uglify-js)
            ++ lib.optional doCompression brotli)
            extraNativeBuildInputs
          ];

        dontPatch = true;
        dontConfigure = true;

        preBuildPhases = [
          (lib.optionalString doElmFormat "elmFormatPhase")
          "prepareElmHomePhase"
          (lib.optionalString doElmTest "elmTestPhase")
        ];

        elmFormatPhase = lib.optionalString doElmFormat ''
          elm-format ${builtins.concatStringsSep " " elmFormatSourceFiles} --validate
        '';

        prepareElmHomePhase = prepareElmHomeScript { inherit elmLock registryDat; };

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
                      builtins.warn "elm-optimize-level-2 accepts multiple entry files but only processes the first" entry
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

        preInstallPhases = [
          (lib.optionalString doElmReview "elmReviewPhase")
        ];

        elmReviewPhase = lib.optionalString doElmReview ''
          if [ -d review ]; then
            elm-review ${builtins.concatStringsSep " " elmReviewFlags} --offline
          else
            echo "Skipping elm-review since no review/ directory was found"
          fi
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
          (lib.optional doMinification "minificationPhase")
          ++ (lib.optional doCompression "compressionPhase")
          ++ (lib.optional doReporting "reportingPhase")
          ++ (lib.optional doContentHashing "contentHashingPhase")
          ;

        minificationPhase = lib.optional doMinification ''
          ${minifier} "$out/${output}" \
            --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
            | ${minifier} --mangle --output "$out/${outputMin}"
        '';

        compressionPhase = lib.optional doCompression ''
          gzip ${builtins.concatStringsSep " " gzipFlags} -c "$out/${toCompress}" > "$out/${toCompress}.gz"
          brotli ${builtins.concatStringsSep " " brotliFlags} -c "$out/${toCompress}" > "$out/${toCompress}.br"
        '';

        reportingPhase = lib.optionalString doReporting ''
          js="${output}"
          js_size=$(stat -c%s $out/$js)
          echo "Compiled size: $js_size bytes ($js)"

          ${lib.optionalString doMinification ''
            min="${outputMin}"
            min_size=$(stat -c%s $out/$min)
            min_pct=$(( 100 * min_size / js_size ))
            echo "Minified size: $min_size bytes ($min) (''${min_pct}% of compiled)"
          ''}

          ${lib.optionalString doCompression ''
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

        contentHashingPhase = lib.optionalString doContentHashing (
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

            ${if keepFilesWithNoHashInFilenames then "cp" else "mv"} "$file" "$out/$hashedFilename"
            echo ${if keepFilesWithNoHashInFilenames then "Copied" else "Moved"} "$filename" "───>" "$hashedFilename"

            if [ $first_entry -eq 0 ]; then
              echo "," >> "$manifest"
            fi
            first_entry=0

            printf '    "%s": "%s"' "$filename" "$hashedFilename" >> "$manifest"
          done

          echo "" >> "$manifest"
          echo "}" >> "$manifest"

          cp "$manifest" "$out/manifest.json"
          echo "Generated manifest.json"
          '');
      };
}
