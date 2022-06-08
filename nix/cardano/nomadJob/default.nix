{
  inputs,
  cell,
}: {
  ogmios = import ./ogmios.nix {inherit inputs cell;};
  cardano-node = import ./cardano-node.nix {inherit inputs cell;};
  cardano-db-sync = import ./cardano-db-sync.nix {inherit inputs cell;};
  cardano-wallet = import ./cardano-wallet.nix {inherit inputs cell;};
  cardano-submit-api = import ./cardano-submit-api.nix {inherit inputs cell;};
}
