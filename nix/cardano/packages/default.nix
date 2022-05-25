{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs cardano-wallet cardano-db-sync cardano-node cardano-ogmios;
  inherit (inputs.cells) cardano;
  inherit (nixpkgs) lib;
  cardano-node-project =
    (
      cardano-node.legacyPackages.extend (
        prev: final: {
          # FIXME: hack to use the materialized version of haskellBuildUtils from cardano-wallet.
          haskellBuildUtils =
            cardano-wallet
            .legacyPackages
            .pkgs
            .iohk-nix-utils;
        }
      )
    )
    .cardanoNodeProject
    .appendModule {
      # TODO: upstream materialization:
      materialized = ./materialized/cardano-node;
    };
in {
  cardano-node =
    cardano-node-project.hsPkgs.cardano-node.components.exes.cardano-node
    // {
      # dispatch c/o automation/jobs.nix
      passthru = {
        inherit (cardano-node-project.plan-nix.passthru) generateMaterialized;
      };
    };
  cardano-tracer = "TODO";
  cardano-submit-api =
    cardano-node-project
    .hsPkgs
    .cardano-submit-api
    .components
    .exes
    .cardano-submit-api;
  cardano-cli = cardano-node-project.hsPkgs.cardano-cli.components.exes.cardano-cli;
  cardano-ping = cardano-node-project.hsPkgs.network-mux.components.exes.cardano-ping;
  cardano-wallet = cardano-wallet.packages.cardano-wallet;
  cardano-address = cardano-wallet.packages.cardano-address;
  cardano-db-sync = cardano-db-sync.packages.cardano-db-sync;
  bech32 = cardano-node-project.hsPkgs.bech32.components.exes.bech32;
  ogmios = cardano-ogmios.packages.ogmios;
  cardano-config-html-public = let
    publicEnvNames = ["mainnet" "testnet" "vasil-qa"];
    environments = lib.filterAttrs (n: _: builtins.elem n publicEnvNames) cardano.environments;
  in
    cardano.library.generateStaticHTMLConfigs environments;
  cardano-config-html-internal = cardano.library.generateStaticHTMLConfigs cardano.environments;
  carp = import ./carp {inherit inputs;};
}
