############################################################################
# tarball releases
#
# This bundles up the executables its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ stdenv
, lib
, runCommand
, haskellBuildUtils
, bintools
, nix
, zip
, version
, exes
, copyEnvsTemplate
, environments
, name ? "cardano-node-${version}"
}:
runCommand name
{
  buildInputs = [ haskellBuildUtils bintools nix zip ];
} ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./
  chmod -R +w .

  ${lib.optionalString stdenv.hostPlatform.isDarwin (lib.concatMapStrings (exe: ''
    rewrite-libs . ${exe}/bin/*
  '') exes)}

  DATA_DIR="$(pwd)"
  ${copyEnvsTemplate { inherit (environments) mainnet testnet; }}

  ${if stdenv.hostPlatform.isWindows
    then "zip -r $out/${name}.zip ."
    else "tar -czf $out/${name}.tar.gz ."
  }
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
