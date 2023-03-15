{
  description = "Cardano World";

  inputs = {
    std.url = "github:divnix/std";
    data-merge.follows = "std/dmerge";
    n2c.follows = "std/n2c";

    # --- Bitte Stack ----------------------------------------------
    bitte = {
      url = "github:input-output-hk/bitte/bump-glusterfs";
      inputs.capsules.follows = "capsules";
    };

    bitte-cells.url = "github:input-output-hk/bitte-cells";
    tullia.url = "github:input-output-hk/tullia";
    # --------------------------------------------------------------

    # --- Auxiliaries ----------------------------------------------
    nixpkgs.follows = "bitte/nixpkgs";
    nixpkgs-haskell.follows = "haskell-nix/nixpkgs-unstable";

    capsules = {
      url = "github:input-output-hk/devshell-capsules";

      # To obtain latest available bitte-cli
      inputs.bitte.follows = "bitte";
    };

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    openziti.url = "github:johnalotoski/openziti-bins";
    deploy-rs.url = "github:serokell/deploy-rs";
    # --------------------------------------------------------------

    # --- App Supporting Inputs ------------------------------------
    byron-chain = {
      url = "github:input-output-hk/cardano-mainnet-mirror";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-inclusive.url = "github:input-output-hk/nix-inclusive";
    # --------------------------------------------------------------

    # --- Bridge Heads----------------------------------------------
    cardano-db-sync.url = "github:input-output-hk/cardano-db-sync/13.0.4";
    cardano-node.url = "github:input-output-hk/cardano-node/1.35.6";
    cardano-wallet.url = "github:input-output-hk/cardano-wallet/v2022-07-01";

    cardano-explorer-app = {
      url = "github:input-output-hk/cardano-explorer-app/fix-nix-system";
      flake = false;
    };

    cardano-graphql = {
      url = "github:input-output-hk/cardano-graphql";
      flake = false;
    };

    ogmios = {
      url = "github:CardanoSolutions/ogmios/v5.5.5";
      flake = false;
    };
    # --------------------------------------------------------------

    # --- Explorer Specific ----------------------------------------
    explorer-cardano-db-sync.url = "github:input-output-hk/cardano-db-sync/13.0.4";

    explorer-cardano-node.url = "github:input-output-hk/cardano-node/1.35.4";

    explorer-cardano-explorer-app = {
      url = "github:input-output-hk/cardano-explorer-app/1.6.0-mods";
      flake = false;
    };

    explorer-cardano-graphql.url = "github:input-output-hk/cardano-graphql/fixes/incl-test-mod-master";

    explorer-cardano-rosetta.url = "github:input-output-hk/cardano-rosetta/1-8-stable-mods";

    explorer-ogmios.url = "github:input-output-hk/cardano-ogmios/2253ef350822f3d18bf6ac579c5abc2a99c2ac4c";

    explorer-cardano-ops = {
      url = "github:input-output-hk/cardano-ops/master-graphql-mods";
      flake = false;
    };
    # --------------------------------------------------------------
  };
  outputs = inputs: let
    inherit (inputs.nixpkgs) lib;
    cloud = inputs.self.${system}.cloud;
    system = "x86_64-linux";
  in
    inputs.std.growOn {
      inherit inputs;
      cellsFrom = ./nix;
      #debug = ["cells" "cloud" "packages"];
      cellBlocks = [
        (inputs.std.data "constants")
        (inputs.std.data "environments")
        (inputs.std.data "namespaces/infra")
        (inputs.std.data "namespaces/mainnet")
        (inputs.std.data "namespaces/vasil-dev")
        (inputs.std.data "namespaces/shelley-qa")
        (inputs.std.data "namespaces/preprod")
        (inputs.std.data "namespaces/preview")
        (inputs.std.data "namespaces/pv8")
        (inputs.std.data "namespaces/private")
        (inputs.std.data "namespaces/perf")
        (inputs.std.data "alerts")
        (inputs.std.data "dashboards")
        (inputs.std.devshells "devshells")
        (inputs.std.nixago "nixago")
        (inputs.std.functions "bitteProfile")
        (inputs.std.functions "devshellProfiles")
        (inputs.std.functions "hydrationProfiles")
        (inputs.std.functions "library")
        (inputs.std.functions "nomadJob")
        (inputs.std.functions "nomadCharts")
        (inputs.std.containers "oci-images")
        (inputs.std.installables "packages")
        (inputs.std.functions "hydraJobs")
        (inputs.std.functions "prepare-mono-repo")
        (inputs.std.runnables "entrypoints")
        (inputs.std.runnables "healthChecks")
        # automation
        (inputs.std.runnables "jobs")

        # Tullia
        (inputs.tullia.tasks "pipelines")
        (inputs.std.functions "actions")
      ];
    }
    # Soil (layers) ...
    # 1) bitte instrumentation (TODO: `std`ize bitte)
    (
      let
        bitte = inputs.bitte.lib.mkBitteStack {
          inherit inputs;
          inherit (inputs) self;
          domain = "world.dev.cardano.org";
          bitteProfile = inputs.self.${system}.metal.bitteProfile.default;
          hydrationProfile = inputs.self.${system}.cloud.hydrationProfiles.default;
          deploySshKey = "./secrets/ssh-cardano";
        };
      in
        # if the bitte input is silenced (replaced by divnix/blank)
        # then don't generate flake level attrNames from mkBitteStack (it fails)
        if inputs.bitte ? lib
        then bitte
        else {}
    )

    # 2) renders nomad environments (TODO: `std`ize as actions)
    (let
      mkNomadJobs = let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
      in
        builtins.mapAttrs (
          n: job:
            pkgs.linkFarm "job.${n}" [
              {
                name = "job";
                path = pkgs.writeText "${n}.nomad.json" (builtins.toJSON job);
              }
            ]
        );
    in {
      infra = mkNomadJobs cloud."namespaces/infra";
      mainnet = mkNomadJobs cloud."namespaces/mainnet";
      vasil-qa = mkNomadJobs cloud."namespaces/vasil-qa";
      vasil-dev = mkNomadJobs cloud."namespaces/vasil-dev";
      preprod = mkNomadJobs cloud."namespaces/preprod";
      preview = mkNomadJobs cloud."namespaces/preview";
      pv8 = mkNomadJobs cloud."namespaces/pv8";
      private = mkNomadJobs cloud."namespaces/private";
      perf = mkNomadJobs cloud."namespaces/perf";
    })

    # 3) hydra jobs
    (
      let
        jobs = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations") (
          lib.mapAttrs (n: lib.mapAttrs (_: cell: cell.hydraJobs or {})) {
            # systems with hydra builders:
            inherit (inputs.self) x86_64-linux x86_64-darwin;
          }
        );
        requiredJobs = lib.filterAttrsRecursive (n: v: n == "required" || !(lib.isDerivation v)) jobs;
        required = inputs.self.x86_64-linux.automation.jobs.mkHydraRequiredJob [] requiredJobs;
      in {
        hydraJobs =
          jobs
          // {
            inherit required;
          };
      }
    )

    # 4) oci-images re-export due to image tag changes stemming from bitte-cells follows
    {
      vector.oci-images = inputs.std.harvest inputs.bitte-cells ["vector" "oci-images"];
    }

    # 5) tullia tasks and cicero actions
    (inputs.tullia.fromStd {
      actions = inputs.std.harvest inputs.self ["cloud" "actions"];
      tasks = inputs.std.harvest inputs.self ["automation" "pipelines"];
    });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [
      # TODO: spongix
      "https://cache.iog.io"
    ];

    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    # post-build-hook = "./upload-to-cache.sh";
    allow-import-from-derivation = "true";
  };
  # --------------------------------------------------------------
}
