{
  description = "Cardano World";
  inputs.std.url = "github:divnix/std";
  inputs.std.inputs.nixpkgs.follows = "nixpkgs";
  inputs.n2c.url = "github:nlewo/nix2container";
  # TODO: remove when moved to monorepo
  inputs.cardano-node.url = "github:input-output-hk/cardano-node/genesis-create-cardano";
  inputs.data-merge.url = "github:divnix/data-merge";
  inputs = {
    # --- Bitte Stack ----------------------------------------------
    bitte.url = "github:input-output-hk/bitte";
    bitte-cells.url = "github:input-output-hk/bitte-cells";
    # --------------------------------------------------------------
    # --- Auxiliaries ----------------------------------------------
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    capsules.url = "github:input-output-hk/devshell-capsules";
    # --------------------------------------------------------------
  };
  outputs = inputs: let
    nomadEnvs = inputs.self.${system}.cloud.nomadEnvs;
    system = "x86_64-linux";
  in
    inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      #debug = ["cells" "cloud" "packages"];
      organelles = [
        (inputs.std.data "nomadEnvs")
        (inputs.std.data "constants")
        (inputs.std.functions "bitteProfile")
        (inputs.std.functions "oci-images")
        (inputs.std.installables "packages")
        (inputs.std.installables "config-data")
        (inputs.std.functions "hydrationProfile")
        # just repo automation; std - just integration pending
        (inputs.std.runnables "jobs")
        (inputs.std.runnables "entrypoints")
        (inputs.std.devshells "devshells")
      ];
    }
    # soil (TODO: eat up soil)
    (
      let
      in
        inputs.bitte.lib.mkBitteStack {
          inherit inputs;
          inherit (inputs) self;
          domain = "world.dev.cardano.org";
          bitteProfile = inputs.self.${system}.metal.bitteProfile.default;
          hydrationProfile = inputs.self.${system}.cloud.hydrationProfile.default;
          deploySshKey = "./secrets/ssh-cardano";
        }
    ) {
      infra = inputs.bitte.lib.mkNomadJobs "infra" nomadEnvs;
      testnet-prod = inputs.bitte.lib.mkNomadJobs "testnet-prod" nomadEnvs;
      testnet-dev = inputs.bitte.lib.mkNomadJobs "testnet-dev" nomadEnvs;
    };
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [
      # TODO: spongix
      "https://hydra.iohk.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    # post-build-hook = "./upload-to-cache.sh";
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
