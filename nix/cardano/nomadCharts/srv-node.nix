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
