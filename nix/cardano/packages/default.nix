{ inputs
, cell
,
}:
let
  inherit
    (inputs)
    self
    std
    nixpkgs
    iohk-nix
    cardano-node
    cardano-wallet
    cardano-db-sync
    ogmios
    cardano-graphql
    cardano-explorer-app
    nix-inclusive
    ;
  inherit (inputs.cells) cardano;
  inherit (nixpkgs) lib;

in lib.makeOverridable ({ evalSystem ? nixpkgs.system }: let
  inherit
    (import inputs.nixpkgs-haskell {
      inherit (nixpkgs) system;
      inherit (inputs.haskell-nix) config;
      overlays = with iohk-nix.overlays; [
        inputs.haskell-nix.overlay
        haskell-nix-extra
        crypto
        (final: prev: {
          haskellBuildUtils = prev.haskellBuildUtils.override {
            inherit compiler-nix-name index-state;
          };
        })
      ];
    })
    haskell-nix
    ;

  inherit (haskell-nix) haskellLib;

  project =
    (import ./haskell.nix {
      inherit lib haskell-nix evalSystem;
      inherit (inputs) byron-chain;
      src = self;
      inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };
    });

  inherit (project.args) compiler-nix-name;
  inherit (project) index-state;

  ogmiosProject = project.appendModule {
    name = "ogmios";
    gitrev = ogmios.rev;
    src = haskellLib.cleanSourceWith {
      name = "ogmios-src";
      src = ogmios;
      subDir = "server";
      filter = path: type:
        builtins.all (x: x) [
          (baseNameOf path != "package.yaml")
        ];
    };
    modules = [
      {
        doHaddock = lib.mkForce false;
        doCheck = lib.mkForce false;
      }
    ];
  };
in
{
  inherit project ogmiosProject;
  inherit (cardano-node.packages) cardano-node cardano-cli cardano-submit-api cardano-tracer cardano-ping bech32 db-synthesizer;
  inherit (project.exes) cardano-new-faucet;
  inherit (cardano-wallet.packages) cardano-wallet;
  inherit (cardano-wallet.packages) cardano-address;
  inherit (cardano-db-sync.packages) cardano-db-sync;
  inherit (ogmiosProject.hsPkgs.ogmios.components.exes) ogmios;
  cardano-graphql = (import (cardano-graphql + "/nix/pkgs.nix") { inherit (nixpkgs) system; }).packages.cardano-graphql;
  graphql-engine = (import (cardano-graphql + "/nix/pkgs.nix") { inherit (nixpkgs) system; }).packages.graphql-engine;
  cardano-explorer-app =
    let
      # TODO fix the ugliness to make this work
      package = nixpkgs.callPackage (cardano-explorer-app + "/nix/cardano-explorer-app.nix") {
        nix-inclusive = nix-inclusive.lib.inclusive;
        sources = null;
      };
    in
    package;
  #cardano-rosetta-server = (import (cardano-rosetta + "/nix/pkgs.nix") {inherit (nixpkgs) system;}).packages.cardano-rosetta-server;
  cardano-config-html-public =
    let
      publicEnvNames = [ "mainnet" "testnet" "shelley_qa" "vasil-dev" ];
      environments = lib.filterAttrs (_: v: !v.private) cardano.environments;
    in
    cardano.library.generateStaticHTMLConfigs environments;
  cardano-config-html-internal = cardano.library.generateStaticHTMLConfigs cardano.environments;
  inherit nix-inclusive; # TODO REMOVE
}) {}
