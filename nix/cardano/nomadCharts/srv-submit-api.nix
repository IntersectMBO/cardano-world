{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  check = [
    {
      address_mode = "host";
      interval = "1m0s";
      port = "submit";
      timeout = "2s";
      type = "tcp";
    }
  ];
  name = "${namespace}-submit";
  port = "submit";
  tags = ["\${NOMAD_ALLOC_ID}"];
}
