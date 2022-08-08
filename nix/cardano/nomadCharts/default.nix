{
  inputs,
  cell,
}: {
  ogmios = import ./ogmios.nix {inherit inputs cell;};
  cardano-node = import ./cardano-node.nix {inherit inputs cell;};
  cardano-db-sync = import ./cardano-db-sync.nix {inherit inputs cell;};
  cardano-faucet = import ./cardano-faucet.nix {inherit inputs cell;};
  cardano-wallet = import ./cardano-wallet.nix {inherit inputs cell;};
  oura = import ./oura.nix {inherit inputs cell;};
}
