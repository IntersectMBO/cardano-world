{
  namespace,
  jobname,
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
  name = "${namespace}-${jobname}-node";
  port = "node";
}
