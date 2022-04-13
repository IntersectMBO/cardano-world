{
  inputs,
  cell,
}: let
  inherit (inputs) nixpkgs;
  inherit (inputs.bitte-cells._writers.library) writeShellApplication;
  inherit (cell) packages;
in {
  cardano-node =
    writeShellApplication {
      name = "entrypoint";
      text = ''
        # Exit if any required variables are not set
        [ -z "''${DATA_DIR:-}" ] && echo "DATA_DIR env var must be set -- aborting" && exit 1
        # TODO: Need more logic here if config/topology are not set e.g. mainnet/testnet
        [ -z "''${NODE_CONFIG:-}" ] && echo "NODE_CONFIG env var must be set -- aborting" && exit 1
        [ -z "''${NODE_TOPOLOGY:-}" ] && echo "NODE_TOPOLOGY env var must be set -- aborting" && exit 1

        # Build args array
        args+=("--config" "$NODE_CONFIG")
        args+=("--database-path" "$DATA_DIR/db")
        args+=("--topology" "$NODE_TOPOLOGY")
        [ -n "''${HOST_ADDR:-}" ] && args+=("--host-addr" "$HOST_ADDR")
        [ -n "''${HOST_IPV6_ADDR:-}" ] && args+=("--host-ipv6-addr" "$HOST_IPV6_ADDR")
        [ -n "''${PORT:-}" ] && args+=("--port" "$PORT")
        [ -n "''${SOCKET_PATH:-}" ] && args+=("--socket-path" "$SOCKET_PATH")

        # Ignore RTS flags for now. Need to figure out best way to pass a list
        #[ -n "''${RTS_FLAGS:-}" ] && args+=$RTS_FLAGS

        [ -n "''${BYRON_DELEG_CERT:-}" ] && args+=("--byron-delegation-certificate" "$BYRON_DELEG_CERT")
        [ -n "''${BYRON_SIGNING_KEY:-}" ] && args+=("--byron-signing-key" "$BYRON_SIGNING_KEY")
        [ -n "''${SHELLEY_KES_KEY:-}" ] && args+=("--shelley-kes-key" "$SHELLEY_KES_KEY")
        [ -n "''${SHELLEY_VRF_KEY:-}" ] && args+=("--shelley-vrf-key" "$SHELLEY_VRF_KEY")
        [ -n "''${SHELLEY_OPCERT:-}" ] && args+=("--shelley-operational-certificate" "$SHELLEY_OPCERT")

        exec ${packages.cardano-node}/bin/cardano-node run "''${args[@]}"
      '';
    };

}
