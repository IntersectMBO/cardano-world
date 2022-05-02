{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  check = [
    {
      address_mode = "host";
      args = [];
      # FIXME: switch back to fully qualified invocation
      # after: https://github.com/nlewo/nix2container/issues/15
      # command = "${healthChecks.db-sync-network-testnet-sync}/bin/cardano-db-sync-network-testnet-sync-check";
      command = "/bin/cardano-db-sync-network-testnet-sync-check";
      interval = "30s";
      # on_update = "ignore_warnings";
      # check_restart.ignore_warnings = true;
      task = "db-sync";
      timeout = "10s";
      type = "script";
    }
  ];
  name = "${namespace}-dbsync";
}
