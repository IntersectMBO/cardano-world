{
  description = "Cardano World";

  inputs = {
    std = {
      url = "path:/home/jbgi/Dev/iohk/std";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    n2c.url = "github:nlewo/nix2container";
    haskellNix = {
      url = "path:/home/jbgi/Dev/iohk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
      inputs.nixpkgs.follows = "nixpkgs-haskell";
    };
    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    iohkNix = {
      url = "path:/home/jbgi/Dev/iohk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    data-merge.url = "github:divnix/data-merge";
    byron-chain = {
      url = "github:input-output-hk/cardano-mainnet-mirror";
      flake = false;
    };
    # --- Bitte Stack ----------------------------------------------
    bitte.url = "github:input-output-hk/bitte";
    bitte-cells.url = "github:input-output-hk/bitte-cells";
    # --------------------------------------------------------------
    # --- Auxiliaries ----------------------------------------------
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-haskell.follows = "haskellNix/nixpkgs-unstable";
    capsules.url = "github:input-output-hk/devshell-capsules";
    # --------------------------------------------------------------
    # --- Bride Heads ----------------------------------------------
    cardano-db-sync.url = "github:input-output-hk/cardano-db-sync/12.0.1-flake-improvements";
    cardano-wallet.url = "github:input-output-hk/cardano-wallet";
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
        (inputs.std.functions "devshellProfiles")
        # just repo automation; std - just integration pending
        (inputs.std.runnables "jobs")
        (inputs.std.runnables "entrypoints")
        (inputs.std.devshells "devshells")
      ];
      nixpkgsConfig = inputs.haskellNix.config;
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
