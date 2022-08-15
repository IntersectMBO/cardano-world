{
  inputs,
  cell,
}: let
  importAsJson = builtins.readFile;
in {
  application-metrics = importAsJson ./dashboards/application-metrics.json;
  p2p = importAsJson ./dashboards/p2p.json;
  performance = importAsJson ./dashboards/performance.json;
}
