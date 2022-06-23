{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  /** TODO: find way to check oura's health
  check = [
    {
      address_mode = "host";
      args = ["health-check"];
      # FIXME: switch back to fully qualified invocation
      # after: https://github.com/nlewo/nix2container/issues/15
      # command = "${healthChecks.cardano-wallet-network-sync}/bin/healthcheck";
      command = "/bin/oura";
      interval = "30s";
      # on_update = "ignore_warnings";
      # check_restart.ignore_warnings = true;
      timeout = "10s";
      type = "script";
    }
  ];
  */
  task = "oura";
  name = "${namespace}-oura";
  port = "oura";
  tags = ["\${NOMAD_ALLOC_ID}" "${namespace}"];
}
