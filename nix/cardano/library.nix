{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) jq writeText runCommandNoCC lib;

  protNames = {
    RealPBFT = {n = "byron";};
    TPraos = {n = "shelley";};
    Cardano = {
      n = "byron";
      shelley = "shelley";
      alonzo = "alonzo";
    };
  };

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
in rec {
  copyEnvsTemplate = environments: ''
    mkdir -p "$DATA_DIR/config"
    ${
      toString (lib.mapAttrsToList (
          env: value: let
            p = value.consensusProtocol;
          in ''
            mkdir -p "$DATA_DIR/config/${env}"
            ${
              if p != "Cardano"
              then ''
                ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig
                  // {
                    GenesisFile = "${protNames.${p}.n}-genesis.json";
                  }))} > "$DATA_DIR/config/${env}/config.json"
              ''
              else ''
                ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig
                  // {
                    ByronGenesisFile = "${protNames.${p}.n}-genesis.json";
                    ShelleyGenesisFile = "${protNames.${p}.shelley}-genesis.json";
                    AlonzoGenesisFile = "${protNames.${p}.alonzo}-genesis.json";
                  }))} > "$DATA_DIR/config/${env}/config.json"
              ''
            }
            ${lib.optionalString (p == "RealPBFT" || p == "Byron") ''
              cp ${value.nodeConfig.GenesisFile} "$DATA_DIR/config/${env}/${protNames.${p}.n}-genesis.json"
            ''}
            ${lib.optionalString (p == "TPraos") ''
              cp ${value.nodeConfig.GenesisFile} "$DATA_DIR/config/${env}/${protNames.${p}.n}-genesis.json"
            ''}
            ${lib.optionalString (p == "Cardano") ''
              cp ${value.nodeConfig.ShelleyGenesisFile} "$DATA_DIR/config/${env}/${protNames.${p}.shelley}-genesis.json"
              cp ${value.nodeConfig.ByronGenesisFile} "$DATA_DIR/config/${env}/${protNames.${p}.n}-genesis.json"
              cp ${value.nodeConfig.AlonzoGenesisFile} "$DATA_DIR/config/${env}/${protNames.${p}.alonzo}-genesis.json"
            ''}
            ${jq}/bin/jq . < ${mkEdgeTopology {
              edgeNodes = [value.relaysNew];
              valency = 2;
            }} > "$DATA_DIR/config/${env}/topology.json"
          ''
        )
        environments)
    }
  '';
  generateStaticHTMLConfigs = environments: let
    createEnvironmentConfigs = copyEnvsTemplate environments;
    configHtml = environments:
      ''
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
                    ${toString (lib.mapAttrsToList (env: value:
                      ''
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
                    ) environments) }
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
