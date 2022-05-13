{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  check = [
    {
      address_mode = "host";
      interval = "1m0s";
      port = "node";
      timeout = "2s";
      type = "tcp";
    }
    # {
    #   address_mode = "host";
    #   args = [];
    #   # FIXME: switch back to fully qualified invocation
    #   # after: https://github.com/nlewo/nix2container/issues/15
    #   # command = "${healthChecks.cardano-node-network-sync}/bin/healthcheck";
    #   command = "/bin/healthcheck";
    #   interval = "30s";
    #   # on_update = "ignore_warnings";
    #   # check_restart.ignore_warnings = true;
    #   task = "node";
    #   timeout = "10s";
    #   type = "script";
    # }
  ];
  name = "${namespace}-node";
  port = "node";
  tags = [
    "\${NOMAD_ALLOC_ID}"
    "${namespace}"
    "ingress"
    "traefik.enable=true"
    "traefik.tcp.routers.${namespace}-node.rule=HostSNI(`*`)"
    "traefik.tcp.routers.${namespace}-node.entrypoints=${namespace}-node-tcp"
  ];
}
