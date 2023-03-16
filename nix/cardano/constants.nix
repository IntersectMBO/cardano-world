{
  inputs,
  cell,
}: {
  node-snapshots = {
    # https://update-cardano-mainnet.iohk.io/cardano-node-state/index.html#
    mainnet = {
      base_url = "https://update-cardano-mainnet.iohk.io/cardano-node-state";
      file_name = "db-mainnet.tar.gz";
    };
  };
  db-sync-snapshots = {
    # https://update-cardano-mainnet.iohk.io/cardano-db-sync/index.html#13/
    mainnet = {
      base_url = "https://update-cardano-mainnet.iohk.io/cardano-db-sync/13";
      file_name = "db-sync-snapshot-schema-13-block-7981218-x86_64.tgz";
    };
  };
}
