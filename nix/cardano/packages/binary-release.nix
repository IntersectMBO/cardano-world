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
, zip
, version
, exes
, config-data
}:

let
  name = "cardano-world-${version}";

in runCommand name {
    buildInputs = [ zip ];
  } ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./
  DATA_DIR="$(pwd)"
  ${config-data.copyEnvsTemplate { inherit (config-data.environments) mainnet testnet; }}
  chmod -R +w .

  ${if stdenv.hostPlatform.isWindows
    then "zip -r $out/${name}.zip ."
    else "tar -czf $out/${name}.tar.gz ."
  }
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
