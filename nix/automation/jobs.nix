{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs iohkNix;
  inherit (inputs.cells.cardano) packages library nixosProfiles;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (inputs.nixpkgs.lib.strings) fileContents;

  cabal-project-utils = nixpkgs.callPackages iohkNix.utils.cabal-project { };

  # TODO: pass down repository cache-hash key
in {
  regenerate-nix = writeShellApplication {
    name = "regenerate-nix";
    text = ''
        find_up() {
        while [[ \$PWD != / ]] ; do
          if [[ -e "\$1" ]]; then
            echo "\$PWD"
            return
          fi
          cd ..
        done
      }

      toplevel=\$(find_up "cabal.project")

      >&2 echo "Updating sha256 of cabal.project 'source-repository-package's..."
      cabal-project-regenerate

      >&2 echo "Materializing list of project packages and executables..."
      ${packages.project.generatePackagesExesMat} nix/cardano/packages/packages-exes.nix
    '';
    runtimeInputs = [nixpkgs.nix cabal-project-utils.cabalProjectRegenerate];
  };
  cabal-project-check = cabal-project-utils.checkCabalProject;
  hlint-check = nixpkgs.callPackage iohkNix.checks.hlint {
    inherit (packages) hlint;
    inherit (packages.project.args) src;
  };
  stylish-haskell = nixpkgs.callPackage inputs.iohkNix.checks.stylish-haskell {
    inherit (packages) stylish-haskell;
    inherit (packages.project.args) src;
  };
  shell = nixpkgs.callPackage inputs.iohkNix.checks.shell {
    src = inputs.self;
  };
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
