{
  namespace,
}: {
  address_mode = "auto";
  check = [
    {
      name = "live";
      address_mode = "host";
      port = "webhook";
      timeout = "2s";
      type = "tcp";
      interval = "1m0s";
    }
  ];
  name = "${namespace}-metadata-webhook";
  port = "webhook";
  tags = [];
}
