{
  inputs,
  cell,
}: {
  consul-workload-policy = {
    tf.hydrate-cluster.configuration.locals.policies.consul.cardano = {
      key_prefix."config/cardano" = {
        policy = "read";
        intentions = "deny";
      };
      session_prefix."" = {
        policy = "write";
        intentions = "deny";
      };
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `cardano` role
    services.vault.policies.client = {
      path."consul/creds/cardano".capabilities = ["read"];
    };
  };
  vault-workload-policy = {
    tf.hydrate-cluster.configuration.locals.policies.vault.cardano = {
      path."kv/data/cardano/*".capabilities = ["read" "list"];
      path."kv/metadata/cardano/*".capabilities = ["read" "list"];
      path."consul/creds/cardano".capabilities = ["read"];
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `cardano` role
    services.vault.policies.client = {
      path."auth/token/create/cardano".capabilities = ["update"];
      path."auth/token/roles/cardano".capabilities = ["read"];
    };
  };
}
