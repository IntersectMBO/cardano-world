{
  inputs,
  cell,
}: let
  inherit (inputs.nixpkgs) jq writeText runCommand lib;
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
in {
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
}
