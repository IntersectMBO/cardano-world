{
  inputs,
  cell,
}: {
  consul-workload-policy.tf.hydrate-cluster.configuration.consul.cardano = {
    key_prefix."config/cardano/*" = { policy = "read"; intentions = "deny"; };
    session_prefix."" = { policy = "write"; intentions = "deny"; };
  };
  vault-workload-policy.tf.hydrate-cluster.configuration.vault.cardano = {
    path."kv/data/cardano/*".capabilities = [ "read", "list" ];
    path."kv/metadata/cardano/*".capabilities = [ "read", "list" ];
  };
}
