{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.cells.cardano) packages library nixosProfiles;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.nixpkgs.lib.strings) fileContents;
  # TODO: pass down repository cache-hash key
in {
  # TODO: script to update materialization:
  # $ nix build .\#cardano-node.passthru.generateMaterialized
  # $ ./result cells/cardano/packages/materialized
  node-2nix = packages.cardano-node.passthru.generateMaterialized;
  run-testnet-node = let
    envName = "testnet";
    config =
      library.evalNodeConfig envName
      nixosProfiles.run-node-testnet;
  in
    writeShellApplication {
      name = "run-cardano-node-testnet";
      text =
        (fileContents ./../entrypoints/node-entrypoint.sh)
        + "\n"
        + config.script;
      env = {
        inherit (config) stateDir socketPath;
        inherit envName;
      };
      runtimeInputs = [
        packages.cardano-node
        packages.cardano-cli
        # TODO: take from somewhere else than aws, e.g. an iohk hydra published path or similar
        nixpkgs.awscli2
        nixpkgs.gnutar
        nixpkgs.gzip
      ];
    };
  push-testnet-node-snapshot = let
    envName = "testnet";
    config =
      library.evalNodeConfig envName
      nixosProfiles.run-node-testnet;
  in
    writeShellApplication {
      name = "push-cardano-node-snapshot-testnet";
      text = fileContents ./push-node-snapshot.sh;
      env = {
        inherit (config) stateDir;
        inherit envName;
      };
      runtimeInputs = [nixpkgs.awscli2 nixpkgs.gnutar nixpkgs.gzip nixpkgs.coreutils];
    };
}
