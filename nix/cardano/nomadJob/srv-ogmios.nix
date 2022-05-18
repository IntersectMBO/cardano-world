{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  check = [
    {
      address_mode = "host";
      args = ["health-check"];
      # FIXME: switch back to fully qualified invocation
      # after: https://github.com/nlewo/nix2container/issues/15
      # command = "${healthChecks.cardano-wallet-network-sync}/bin/healthcheck";
      command = "/bin/ogmios";
      interval = "30s";
      # on_update = "ignore_warnings";
      # check_restart.ignore_warnings = true;
      timeout = "10s";
      type = "script";
    }
  ];
  task = "ogmios";
  name = "${namespace}-ogmios";
  port = "ogmios";
  tags = ["\${NOMAD_ALLOC_ID}" "${namespace}"];
}
