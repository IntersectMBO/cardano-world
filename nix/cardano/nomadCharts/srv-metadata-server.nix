{
  namespace,
}: {
  address_mode = "auto";
  check = [
    {
      name = "live";
      address_mode = "host";
      port = "server";
      timeout = "2s";
      type = "tcp";
      interval = "1m0s";
    }
  ];
  name = "${namespace}-metadata-server";
  port = "server";
  tags = [];
}
