{
  inputs,
  cell,
}: {
  # Cardano Node
  workload-policies-cardano = {
    tf.hydrate-cluster.configuration.locals.policies = {
      consul.cardano = {
        key_prefix."config/cardano" = {
          policy = "read";
          intentions = "deny";
        };
        session_prefix."" = {
          policy = "write";
          intentions = "deny";
        };
      };
      vault.cardano = {
        path."kv/data/cardano/*".capabilities = ["read" "list"];
        path."kv/metadata/cardano/*".capabilities = ["read" "list"];
        path."consul/creds/cardano".capabilities = ["read"];
      };
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `cardano` role
    services.vault.policies.client = {
      path."consul/creds/cardano".capabilities = ["read"];
      path."auth/token/create/cardano".capabilities = ["update"];
      path."auth/token/roles/cardano".capabilities = ["read"];
    };
  };

  # Ogmios
  workload-policies-ogmios = {
    tf.hydrate-cluster.configuration.locals.policies = {
      consul.ogmios = {
        # ogmios also needs to read the cardano config
        key_prefix."config/cardano" = {
          policy = "read";
          intentions = "deny";
        };
        session_prefix."" = {
          policy = "write";
          intentions = "deny";
        };
      };
      vault.ogmios = {
        path."consul/creds/ogmios".capabilities = ["read"];
      };
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `ogmios` role
    services.vault.policies.client = {
      path."consul/creds/ogmios".capabilities = ["read"];
      path."auth/token/create/ogmios".capabilities = ["update"];
      path."auth/token/roles/ogmios".capabilities = ["read"];
    };
  };

  # Oura
  workload-policies-oura = {
    tf.hydrate-cluster.configuration.locals.policies = {
      consul.oura = {
        # oura also needs to read the cardano config
        key_prefix."config/cardano" = {
          policy = "read";
          intentions = "deny";
        };
        session_prefix."" = {
          policy = "write";
          intentions = "deny";
        };
      };
    };
  };

  # Db Sync
  workload-policies-db-sync = {
    tf.hydrate-cluster.configuration.locals.policies = {
      consul.db-sync = {
        # db-sync also needs to read the cardano config
        key_prefix."config/cardano" = {
          policy = "read";
          intentions = "deny";
        };
        session_prefix."" = {
          policy = "write";
          intentions = "deny";
        };
      };
      vault.db-sync = {
        path."kv/data/db-sync/*".capabilities = ["read" "list"];
        path."kv/metadata/db-sync/*".capabilities = ["read" "list"];
        path."consul/creds/db-sync".capabilities = ["read"];
      };
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `db-sync` role
    services.vault.policies.client = {
      path."consul/creds/db-sync".capabilities = ["read"];
      path."auth/token/create/db-sync".capabilities = ["update"];
      path."auth/token/roles/db-sync".capabilities = ["read"];
    };
  };

  # Wallet
  workload-policies-wallet = {
    tf.hydrate-cluster.configuration.locals.policies = {
      consul.wallet = {
        # wallet also needs to read the cardano config
        key_prefix."config/cardano" = {
          policy = "read";
          intentions = "deny";
        };
        session_prefix."" = {
          policy = "write";
          intentions = "deny";
        };
      };
      vault.wallet = {
        path."kv/data/wallet/*".capabilities = ["read" "list"];
        path."kv/metadata/wallet/*".capabilities = ["read" "list"];
        path."consul/creds/wallet".capabilities = ["read"];
      };
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `wallet` role
    services.vault.policies.client = {
      path."consul/creds/wallet".capabilities = ["read"];
      path."auth/token/create/wallet".capabilities = ["update"];
      path."auth/token/roles/wallet".capabilities = ["read"];
    };
  };

  # Submit API
  workload-policies-submit-api = {
    tf.hydrate-cluster.configuration.locals.policies = {
      consul.submit-api = {
        # submit-api also needs to read the cardano config
        key_prefix."config/cardano" = {
          policy = "read";
          intentions = "deny";
        };
        session_prefix."" = {
          policy = "write";
          intentions = "deny";
        };
      };
      vault.submit-api = {
        path."consul/creds/submit-api".capabilities = ["read"];
      };
    };
    # FIXME: consolidate policy reconciliation loop with TF
    # PROBLEM: requires bootstrapper reconciliation loop
    # clients need the capability to impersonate the `submit-api` role
    services.vault.policies.client = {
      path."consul/creds/submit-api".capabilities = ["read"];
      path."auth/token/create/submit-api".capabilities = ["update"];
      path."auth/token/roles/submit-api".capabilities = ["read"];
    };
  };
}
