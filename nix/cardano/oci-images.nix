{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells) _utils;
  inherit (cell) entrypoints packages healthChecks;
  n2c = inputs.n2c.packages.nix2container;

  rootCACerts = nixpkgs.linkFarm "root-ca-certs" [
    {
      name = "etc/ssl/certs/ca-bundle.crt";
      path = "${nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    }
    {
      name = "etc/ssl/certs/ca-certificates.crt";
      path = "${nixpkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    }
  ];

  buildDebugImage = ep: o: n2c.buildImage (_utils.library.mkDebugOCI ep o);
in {
  cardano-node = buildDebugImage entrypoints.cardano-node {
    name = "registry.ci.iog.io/cardano-node";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-node.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-node];})
    ];
    copyToRoot = [nixpkgs.bashInteractive nixpkgs.iana-etc packages.cardano-node packages.cardano-cli];
    config.Cmd = [
      "${entrypoints.cardano-node}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-node-nokv = buildDebugImage entrypoints.cardano-node {
    # name = "registry.ci.iog.io/cardano-node-nokv";
    name = "docker.io/kranium/cardano-node-nokv";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-node-nokv.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-node];})
    ];
    copyToRoot = [nixpkgs.bashInteractive nixpkgs.iana-etc packages.cardano-node packages.cardano-cli];
    config.Cmd = [
      "${entrypoints.cardano-node-nokv}/bin/entrypoint"
    ];
    # FIXME: not sure how to run this with docker locally with perms
    # config.User = "1000:1000";
  };
  cardano-db-sync = buildDebugImage entrypoints.cardano-db-sync {
    name = "registry.ci.iog.io/cardano-db-sync";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-db-sync.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-db-sync];})
    ];
    copyToRoot = [nixpkgs.bashInteractive nixpkgs.iana-etc rootCACerts];
    config.Cmd = [
      "${entrypoints.cardano-db-sync}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-wallet = buildDebugImage entrypoints.cardano-wallet {
    name = "registry.ci.iog.io/cardano-wallet";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-wallet.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-wallet];})
    ];
    copyToRoot = [nixpkgs.bashInteractive nixpkgs.iana-etc healthChecks.cardano-wallet-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-wallet}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-submit-api = buildDebugImage entrypoints.cardano-submit-api {
    name = "registry.ci.iog.io/cardano-submit-api";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-submit-api.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-submit-api];})
    ];
    copyToRoot = [nixpkgs.bashInteractive nixpkgs.iana-etc healthChecks.cardano-submit-api-network-sync];
    config.Cmd = [
      "${entrypoints.cardano-submit-api}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  ogmios = buildDebugImage entrypoints.ogmios {
    name = "registry.ci.iog.io/ogmios";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.ogmios.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.ogmios];})
    ];
    copyToRoot = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.ogmios}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
  cardano-faucet = buildDebugImage entrypoints.cardano-faucet {
    name = "registry.ci.iog.io/cardano-faucet";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = [packages.cardano-new-faucet];})
    ];
    copyToRoot = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.cardano-faucet}/bin/entrypoint"
    ];
    config.User = "1000:1000";
  };
}
