{pkgs, config, ...}:
{
  services.cardano-db-sync.additionalDbUsers = [
    "cardano-graphql"
    "smash"
    "cardano-rosetta-server"
    "dump-registered-relays-topology"
  ];
}
