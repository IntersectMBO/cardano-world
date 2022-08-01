{
  namespace,
  healthChecks,
}: {
  address_mode = "auto";
  check = [];
  name = "${namespace}-dbsync";
  tags = ["\${NOMAD_ALLOC_ID}"];
}
