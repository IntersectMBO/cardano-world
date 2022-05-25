{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) jq writeText runCommandNoCC lib;

  mkEdgeTopology = {
    hostAddr ? "127.0.0.1",
    port ? 3001,
    edgeHost ? "127.0.0.1",
    edgeNodes ? [],
    edgePort ?
      if (edgeNodes != [])
      then 3001
      else
        (
          if edgeHost == "127.0.0.1"
          then 7777
          else 3001
        ),
    valency ? 1,
  }: let
    mkProducers =
      map (edgeHost': {
        addr = edgeHost';
        port = edgePort;
        inherit valency;
      })
      edgeNodes;
    topology = {
      Producers =
        if (edgeNodes != [])
        then mkProducers
        else [
          {
            addr = edgeHost;
            port = edgePort;
            inherit valency;
          }
        ];
    };
  in
    builtins.toFile "topology.yaml" (builtins.toJSON topology);
  mkEdgeTopologyP2P = {
    edgeNodes ? [
      {
        addr = "127.0.0.1";
        port = 3001;
      }
    ],
    useLedgerAfterSlot ? 0,
  }: let
    mkPublicRootsAccessPoints =
      map (edgeNode: {
        address = edgeNode.addr;
        port = edgeNode.port;
      })
      edgeNodes;
    topology = {
      LocalRoots = {
        groups = [
          {
            localRoots = {
              accessPoints = [];
              advertise = false;
            };
            valency = 1;
          }
        ];
      };
      PublicRoots = [
        {
          publicRoots = {
            accessPoints = mkPublicRootsAccessPoints;
            advertise = false;
          };
        }
      ];
      inherit useLedgerAfterSlot;
    };
  in
    builtins.toFile "topology.yaml" (builtins.toJSON topology);
in rec {
  copyEnvsTemplate = environments: let
    mkTopology = env: let
      legacyTopology = mkEdgeTopology {
        edgeNodes = [env.relaysNew];
        valency = 2;
      };
      p2pTopology = mkEdgeTopologyP2P {
        inherit (env) edgeNodes;
        useLedgerAfterSlot = env.usePeersFromLedgerAfterSlot;
      };
    in
      if (env.nodeConfig.EnableP2P or false)
      then p2pTopology
      else legacyTopology;
  in ''
    mkdir -p "$DATA_DIR/config"
    ${
      toString (lib.mapAttrsToList (
          env: value: let
            p = value.consensusProtocol;
          in ''
            mkdir -p "$DATA_DIR/config/${env}"
            ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig
              // {
                ByronGenesisFile = "byron-genesis.json";
                ShelleyGenesisFile = "shelley-genesis.json";
                AlonzoGenesisFile = "alonzo-genesis.json";
              }))} > "$DATA_DIR/config/${env}/config.json"
            ${jq}/bin/jq . < ${__toFile "${env}-db-sync-config.json" (__toJSON (value.dbSyncConfig
              // {
                NodeConfigFile = "config.json";
              }))} > "$DATA_DIR/config/${env}/db-sync-config.json"
            ${jq}/bin/jq . < ${__toFile "${env}-submit-api-config.json" (__toJSON value.submitApiConfig)} > "$DATA_DIR/config/${env}/submit-api-config.json"
            cp ${value.nodeConfig.ByronGenesisFile} "$DATA_DIR/config/${env}/byron-genesis.json"
            cp ${value.nodeConfig.ShelleyGenesisFile} "$DATA_DIR/config/${env}/shelley-genesis.json"
            cp ${value.nodeConfig.AlonzoGenesisFile} "$DATA_DIR/config/${env}/alonzo-genesis.json"
            ${jq}/bin/jq . < ${mkTopology value} > "$DATA_DIR/config/${env}/topology.json"
          ''
        )
        environments)
    }
  '';
  generateStaticHTMLConfigs = environments: let
    createEnvironmentConfigs = copyEnvsTemplate environments;
    configHtml = environments: ''
      <!DOCTYPE html>
      <html>
        <head>
          <title>Cardano Configurations</title>
          <meta charset="UTF-8">
          <meta name="viewport" content="width=device-width, initial-scale=1">
          <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css">
          <script defer src="https://use.fontawesome.com/releases/v5.3.1/js/all.js"></script>
        </head>
        <body>
          <section class="hero is-small is-primary">
            <div class="hero-body">
              <div class="container">
                <h1 class="title is-1">
                  Cardano
                </h1>
                <h2 class="subtitle is-3">
                  Configurations
                </h2>
              </div>
            </div>
          </section>

          <section class="section">
            <div class="container">
              <div class="table-container">
                <table class="table is-narrow is-fullwidth">
                  <thead>
                    <tr>
                      <th>Cluster</th>
                      <th>Config</th>
                    </tr>
                  </thead>
                  <tbody>
                    ${toString (lib.mapAttrsToList (
          env: value: ''
            <tr>
              <td>${env}</td>
              <td>
                <div class="buttons has-addons">
                  <a class="button is-primary" href="config/${env}/config.json">config</a>
                  <a class="button is-info" href="config/${env}/byron-genesis.json">Byron Genesis</a>
                  <a class="button is-info" href="config/${env}/shelley-genesis.json">Shelley Genesis</a>
                  <a class="button is-info" href="config/${env}/alonzo-genesis.json">Alonzo Genesis</a>
                  <a class="button is-info" href="config/${env}/topology.json">topology</a>
                  <a class="button is-primary" href="config/${env}/db-sync-config.json">db-sync config</a>
                  <a class="button is-primary" href="config/${env}/submit-api-config.json">submit-api config</a>
                </div>
              </td>
            </tr>
          ''
        )
        environments)}
                  </tbody>
                </table>
              </div>
            </div>
          </section>
        </body>
      </html>
    '';
  in
    runCommandNoCC "environments.html" {} ''
      DATA_DIR="$out"
      ENVIRONMENTS=(${lib.escapeShellArgs (builtins.attrNames environments)})
      ${createEnvironmentConfigs}
      for env in "''${ENVIRONMENTS[@]}"
      do
        pushd "$out/config/$env"
          find ./
        popd
      done
      cp ${writeText "config.html" (configHtml environments)} $out/index.html
    '';
}
