{ elmHome, elmVersion }:

drv: # A derivation of a patched package, i.e. one created with mkPatch
let
  packagesDir = "${elmHome}/${elmVersion}/packages";
  to = "${packagesDir}/${drv.path}";
  from = "${drv}/${drv.path}";
in
''
if [ -d "${to}" ]; then
  echo "Patching from ${from} to ${to}"

  rm -r "${to}"
  cp -R "${from}" "${to}"
  chmod -R +w "${to}"
else
  echo "Skipping patching from ${from} to ${to} since the destination does not exist"
  echo "Are you sure you installed ${drv.path}?"
fi
''
