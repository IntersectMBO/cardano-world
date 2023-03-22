{
  namespace,
  instance,
}: {
  address_mode = "auto";
  check = [
    {
      name = "live";
      address_mode = "host";
      port = "server${toString instance}";
      type = "tcp";
      interval = "10s";
      timeout = "5s";
    }
  ];
  name = "${namespace}-metadata-server${toString instance}";
  port = "server${toString instance}";
  tags = [
    "\${NOMAD_ALLOC_ID}"
  ];
}
