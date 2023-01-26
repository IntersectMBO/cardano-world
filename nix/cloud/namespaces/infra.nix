{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge;
  inherit (inputs.bitte-cells) patroni;
  inherit (inputs.cells) docs;
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
}
