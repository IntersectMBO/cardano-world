{
  inputs,
  cell,
}: let
  importAsJson = builtins.readFile;
in {
  application-metrics = importAsJson ./dashboards/application-metrics.json;
  dbsync = importAsJson ./dashboards/dbsync.json;
  faucet = importAsJson ./dashboards/faucet.json;
  nginx-basic = importAsJson ./dashboards/nginx-basic.json;
  nginx-vts = importAsJson ./dashboards/nginx-vts.json;
  node-exporter = importAsJson ./dashboards/node-exporter.json;
  p2p = importAsJson ./dashboards/p2p.json;
  performance = importAsJson ./dashboards/performance.json;
  varnish = importAsJson ./dashboards/varnish.json;
  wireguard = importAsJson ./dashboards/wireguard.json;
}
