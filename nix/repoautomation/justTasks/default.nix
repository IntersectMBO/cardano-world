{
  inputs,
  cell,
}: let
  make = inputs.std.std.lib.fromMakesWith inputs;
in {createWallet = make ./createWallet/main.nix {};}
