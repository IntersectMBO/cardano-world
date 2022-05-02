{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (cell) packages library nixosProfiles;
  inherit (inputs.cells._writers.library) writeShellApplication;
  inherit (inputs.nixpkgs.lib.strings) fileContents;
in {
  cardano-node-sync = writeShellApplication {
    name = "cardano-node-sync-check";
    text = fileContents ./node-network-sync-check.sh;
    runtimeInputs = [packages.cardano-cli nixpkgs.jq nixpkgs.coreutils];
  };
}
