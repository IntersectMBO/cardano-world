{
  description = "Cardano World";
  inputs.std.url = "github:divnix/std";
  inputs.std.inputs.nixpkgs.follows = "nixpkgs";
  inputs.n2c.url = "github:nlewo/nix2container";
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
    # --- Bride Heads ----------------------------------------------
    # TODO: remove when moved to monorepo
    cardano-node.url = "github:input-output-hk/cardano-node";
    cardano-db-sync.url = "github:input-output-hk/cardano-db-sync/vasil-qa-deploy-0";
    cardano-wallet.url = "github:input-output-hk/cardano-wallet";
    cardano-ogmios.url = "github:input-output-hk/cardano-ogmios";
    # --------------------------------------------------------------
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    naersk.url = "github:nix-community/naersk";
    naersk.inputs.nixpkgs.follows = "nixpkgs";
    carp = {
      url = "github:dcSpark/carp";
      flake = false;
    };
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
        (inputs.std.data "constants")
        (inputs.std.data "environments")
        (inputs.std.data "nomadEnvs")
        (inputs.std.devshells "devshells")
        (inputs.std.functions "bitteProfile")
        (inputs.std.functions "devshellProfiles")
        (inputs.std.functions "hydrationProfiles")
        (inputs.std.functions "library")
        (inputs.std.functions "nomadJob")
        (inputs.std.functions "oci-images")
        (inputs.std.installables "packages")
        (inputs.std.runnables "entrypoints")
        (inputs.std.runnables "healthChecks")
        # automation
        (inputs.std.runnables "jobs")
        (inputs.std.functions "pipelines")
      ];
    }
    # Soil (layers) ...
    # 1) bitte instrumentation (TODO: `std`ize bitte)
    (
      let
      in
        inputs.bitte.lib.mkBitteStack {
          inherit inputs;
          inherit (inputs) self;
          domain = "world.dev.cardano.org";
          bitteProfile = inputs.self.${system}.metal.bitteProfile.default;
          hydrationProfile = inputs.self.${system}.cloud.hydrationProfiles.default;
          deploySshKey = "./secrets/ssh-cardano";
        }
    )
    # 2) renderes nomad environments (TODO: `std`ize as actions)
    {
      infra = inputs.bitte.lib.mkNomadJobs "infra" nomadEnvs;
      vasil-qa = inputs.bitte.lib.mkNomadJobs "vasil-qa" nomadEnvs;
    };
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [
      # TODO: spongix
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "cache.iog.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    # post-build-hook = "./upload-to-cache.sh";
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
