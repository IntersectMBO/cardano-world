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
  tmpdir = nixpkgs.runCommand "tmpdir" {} "mkdir -p $out/tmp";
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
    config.User = "65534:65534";
    config.StopSignal = "INT";
  };

  cardano-db-sync = buildDebugImage entrypoints.cardano-db-sync {
    name = "registry.ci.iog.io/cardano-db-sync";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = entrypoints.cardano-db-sync.runtimeInputs;})
      (n2c.buildLayer {deps = [packages.cardano-db-sync];})
    ];
    copyToRoot = [nixpkgs.bashInteractive nixpkgs.iana-etc rootCACerts tmpdir];
    config.Cmd = [
      "${entrypoints.cardano-db-sync}/bin/entrypoint"
    ];
    config.User = "65534:65534";
    perms = [
      {
        path = tmpdir;
        regex = ".*";
        mode = "1777";
      }
    ];
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
    config.User = "65534:65534";
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
    config.User = "65534:65534";
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
    config.User = "65534:65534";
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
    config.User = "65534:65534";
  };

  metadata-server = buildDebugImage entrypoints.metadata-server {
    name = "registry.ci.iog.io/metadata-server";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = [packages.metadata-server];})
    ];
    copyToRoot = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.metadata-server}/bin/entrypoint"
    ];
    config.User = "65534:65534";
  };

  metadata-sync = buildDebugImage entrypoints.metadata-sync {
    name = "registry.ci.iog.io/metadata-sync";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = [packages.metadata-sync];})
    ];
    copyToRoot = with nixpkgs; [bashInteractive cacert];
    config.Cmd = [
      "${entrypoints.metadata-sync}/bin/entrypoint"
    ];
    config.User = "65534:65534";
  };

  metadata-varnish = buildDebugImage entrypoints.metadata-varnish {
    name = "registry.ci.iog.io/metadata-varnish";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = with nixpkgs; [varnish varnishPackages.modules];})
    ];
    copyToRoot = [nixpkgs.bashInteractive];
    config.Cmd = [
      "${entrypoints.metadata-varnish}/bin/entrypoint"
    ];
    config.User = "65534:65534";
  };

  metadata-webhook = buildDebugImage entrypoints.metadata-webhook {
    name = "registry.ci.iog.io/metadata-webhook";
    maxLayers = 25;
    layers = [
      (n2c.buildLayer {deps = [packages.metadata-webhook];})
    ];
    copyToRoot = with nixpkgs; [bashInteractive cacert];
    config.Cmd = [
      "${entrypoints.metadata-webhook}/bin/entrypoint"
    ];
    config.User = "65534:65534";
  };
}
