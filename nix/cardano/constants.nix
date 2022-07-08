{
  inputs,
  cell,
}: {
  node-snapshots = {
    # https://updates-cardano-testnet.s3.ap-southeast-1.amazonaws.com/cardano-node-state/index.html
    testnet = {
      base_url = "https://updates-cardano-testnet.s3.amazonaws.com/cardano-node-state";
      file_name = "db-testnet.tar.gz";
    };
    # https://update-cardano-mainnet.iohk.io/cardano-node-state/index.html#
    mainnet = {
      base_url = "https://update-cardano-mainnet.iohk.io/cardano-node-state";
      file_name = "db-testnet.tar.gz";
    };
  };
  db-sync-snapshots = {
    # https://updates-cardano-testnet.s3.amazonaws.com/cardano-db-sync/index.html#13/
    testnet = {
      base_url = "https://updates-cardano-testnet.s3.amazonaws.com/cardano-db-sync/13";
      file_name = "db-sync-snapshot-schema-13-block-3680594-x86_64.tgz";
    };
    # https://update-cardano-mainnet.iohk.io/cardano-db-sync/index.html#13/
    mainnet = {
      base_url = "https://update-cardano-mainnet.iohk.io/cardano-db-sync/13";
      file_name = "db-sync-snapshot-schema-13-block-7457020-x86_64.tgz";
    };
  };
}
