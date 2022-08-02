{
  inputs,
  cell,
}: let
  l = inputs.nixpkgs.lib // builtins;
  msg = target: ''
    WARNING: the 'cardano.nomadJob' target is deprecated and has been renamed to 'cardano.nomadCharts'.

    Please use 'cardano.nomadCharts.${target}' instead of 'cardano.nomadJob.${target}'.

    Reason: there has been a semantic inconsistency in that nomadJobs actually where only templates.
    In analogy to helm charts, we now call them nomad charts to clarify their semantics and free up
    nomad job for what it actually is.
  '';
in {
  ogmios = l.warn (msg "ogmios") cell.nomadCharts.ogmios;
  cardano-node = l.warn (msg "cardano-node") cell.nomadCharts.cardano-node;
  cardano-db-sync = l.warn (msg "cardano-db-sync") cell.nomadCharts.cardano-db-sync;
}
