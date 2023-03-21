{
  namespace,
}: {
  address_mode = "auto";
  check = [
    {
      name = "live";
      address_mode = "host";
      port = "server";
      type = "tcp";
      interval = "10s";
      timeout = "5s";
    }
  ];
  name = "${namespace}-metadata-server";
  port = "server";
  tags = [];
}
