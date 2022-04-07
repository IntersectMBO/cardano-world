{
  description = "Cardano Repository top-level development shell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.devshell.url = "github:numtide/devshell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.capsules.url = "github:input-output-hk/devshell-capsules";
  inputs.main.url = "path:../.";
  outputs = inputs:
    inputs.flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (
      system: let
        inherit
          (inputs.main.inputs.std.deSystemize system inputs)
          main
          devshell
          nixpkgs
          capsules
          ;
        inherit
          (main.inputs.std.deSystemize system inputs.main.inputs)
          bitte-cells
          bitte
          std
          ;
        inherit (devshell.legacyPackages) mkShell;
        inherit (main.clusters.cardano-testnet) _proto;

        walletWorld = {
          extraModulesPath,
          pkgs,
          ...
        }: {
          name = nixpkgs.lib.mkForce "Cardano World";
          imports = [
            std.std.devshellProfiles.default
            bitte.devshellModule
          ];
          bitte = {
            domain = "world.dev.cardano.org";
            cluster = "testnet";
            namespace = "testnet-prod";
            provider = "AWS";
            cert = null;
            aws_profile = "cardano";
            aws_region = "eu-central-1";
            aws_autoscaling_groups =
              _proto.config.cluster.awsAutoScalingGroups;
          };
          cellsFrom = "./nix";
        };
      in {
        devShells.dev = mkShell {
          imports = [
            walletWorld
            capsules.base
            capsules.cloud
          ];
        };
        devShells.ops = mkShell {
          imports = [
            walletWorld
            capsules.base
            capsules.cloud
            capsules.hooks
            capsules.metal
            capsules.integrations
            capsules.tools
            bitte-cells.patroni.devshellProfiles.default
            bitte-cells.cardano.devshellProfiles.default
          ];
        };
      }
    );
}
