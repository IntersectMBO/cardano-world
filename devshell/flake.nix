{
  description = "Cardano Repository top-level development shell";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.devshell.url = "github:numtide/devshell?ref=refs/pull/169/head";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.main.url = "path:../.";
  outputs = inputs:
    inputs.flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (
      system: let
        inherit
          (inputs.main.inputs.std.deSystemize system inputs)
          main
          devshell
          nixpkgs
          ;
        inherit
          (main.inputs.std.deSystemize system inputs.main.inputs)
          bitte-cells
          bitte
          std
          ;
        inherit
          (std.deSystemize system bitte.inputs)
          cli
          ;
        inherit (main.clusters.cardano-testnet) _proto;
      in {
        devShells.__default = devshell.legacyPackages.mkShell (
          {
            extraModulesPath,
            pkgs,
            ...
          }: {
            name = nixpkgs.lib.mkForce "Cardano Shell";
            imports = [
              std.std.devshellProfiles.default
              cli.devshellModules.bitte
              bitte-cells.patroni.devshellProfiles.default
              bitte-cells.cardano.devshellProfiles.default
            ];
            bitte = {
              domain = "dev.cardano.org";
              cluster = "testnet";
              namespace = "testnet-prod";
              provider = "AWS";
              cert = null;
              aws_profile = "testnet-prod";
              aws_region = "eu-central-1";
              aws_autoscaling_groups =
                _proto.config.cluster.awsAutoScalingGroups;
            };
            cellsFrom = "./nix";
            packages = [
              # formatters
              nixpkgs.legacyPackages.alejandra
              # nixpkgs.legacyPackages.shfmt
              # nixpkgs.legacyPackages.nodePackages.prettier
            ];
          }
        );
      }
    );
}
