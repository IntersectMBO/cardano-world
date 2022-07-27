{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  check = [
    {
      address_mode = "host";
      type = "http";
      path = "/health";
      interval = "30s";
      timeout = "10s";
    }
  ];
  task = "ogmios";
  name = "${namespace}-ogmios";
  port = "ogmios";
  tags = ["\${NOMAD_ALLOC_ID}" "${namespace}"];
}
