{ dotElmLinks }:

{ elmLock
, registryDat
, directory ? ".elm"
}:

''
echo "Prepare ${directory} and set ELM_HOME=${directory}"
cp -LR "${dotElmLinks { inherit elmLock registryDat; }}" ${directory}
chmod -R +w ${directory}
export ELM_HOME="$PWD/${directory}"
''
