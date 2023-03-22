{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.bitte-cells) patroni;
  inherit (inputs.cells) cardano docs;
  inherit (cell) constants;

  WALG_S3_PREFIX = "s3://iog-cardano-bitte/backups/infra/walg";
in {
  documentation = docs.nomadCharts.documentation constants.envs.infra;
  database = data-merge.merge (patroni.nomadCharts.default (constants.envs.infra
    // {
      datacenters = ["eu-central-1"];
    })) {
    job.database.group.database.task.patroni.env = {inherit WALG_S3_PREFIX;};
    job.database.group.database.task.backup-walg.env = {inherit WALG_S3_PREFIX;};
  };

  metadata = let
    jobname = "metadata";
  in cardano.nomadCharts.metadata (
    constants.envs.infra
    // {
      datacenters = ["eu-central-1"];
      inherit jobname;
      inherit
        (cardano.constants.metadata.testnet)
        varnishTtlSec
        varnishMemoryMb
        varnishMaxPostSizeBodyKb
        varnishMaxPostSizeCachableKb
        ;
    }
  );
}
