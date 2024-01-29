name: wgIP: {self, pkgs, config, lib, etcEncrypted, options, ...}:
let
  inherit (lib) mkOption types;

  maintenanceMode = false;

  deployType = config.currentCoreNode.deployType or config.currentAwsAutoScalingGroup.deployType;
  isSops = builtins.elem deployType ["aws" "awsExt"];

  cfg = config.services.explorer;
  dbSyncCfg = config.services.cardano-db-sync;
  nodeCfg = config.services.cardano-node;
  ogmiosCfg = config.services.cardano-ogmios;

  cardanoLibExplorer = self.inputs.explorer-iohk-nix.pkgs.cardanoLib;
  environments = cardanoLibExplorer.environments;
  environmentConfig = environments.${cfg.environmentName};
  auxConfig = import ./aux-config.nix self.inputs;

  dbSyncPkgs = self.inputs.explorer-cardano-db-sync.legacyPackages.x86_64-linux;
  explorerAppPkgs = (import (self.inputs.explorer-cardano-explorer-app + "/nix/pkgs.nix") {inherit (nodeCfg) system;}).packages;
  graphqlPkgs = self.inputs.explorer-cardano-graphql.${nodeCfg.system}.cardano-graphql.packages;
  hasuraPkgs = self.inputs.explorer-cardano-graphql.${nodeCfg.system}.hasura.packages;
  nodePkgs = self.inputs.explorer-cardano-node.legacyPackages.${nodeCfg.system}.cardanoNodePackages;
  opsPkgs = import (self.inputs.explorer-cardano-ops + "/nix") {inherit (nodeCfg) system;};
  rosettaPkgs = self.inputs.explorer-cardano-rosetta.${nodeCfg.system}.cardano-rosetta.packages;

  cardanoNodeConfigPath = builtins.toFile "cardano-node-config.json" (builtins.toJSON nodeCfg.nodeConfig);

  inherit (dbSyncPkgs) cardano-smash-server-no-basic-auth;
  inherit (nodePkgs) cardano-node cardano-cli;
  inherit (graphqlPkgs) cardano-graphql persistgraphql;
  inherit (hasuraPkgs) graphql-engine hasura-cli hasura-cli-ext;
in {
  imports = [
    (self.inputs.explorer-cardano-graphql + "/nix/nixos")
    (self.inputs.explorer-cardano-rosetta + "/nix/nixos")
    self.inputs.explorer-ogmios.nixosModules.ogmios
  ];

  options = {
    services.explorer = {
      environmentName = mkOption {
        # Required to build the correct environment
        type = types.str;
      };

      explorerHostName = mkOption {
        # Required to build the correct environment
        type = types.str;
        default = if cfg.environmentName == "mainnet"
          then "explorer.cardano.org"
          else "explorer.${environmentConfig.domain}";
      };

      totalMachineMemoryGB = mkOption {
        # Must be supplied to adjust varnish cache memory appropriately.
        type = types.int;
      };

      withSmash = mkOption {
        type = types.bool;
        default = true;
      };

      withSubmitApi = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = {
    services.cardano-db-sync = let
      environmentConfig = let
        inherit
          (environments.${cfg.environmentName})
          networkConfig
          nodeConfig
          ;

        legacyParams = {
          ApplicationName = "cardano-sl";
          ApplicationVersion = if cfg.environmentName == "mainnet" then 1 else 0;
        };

        networkConfig' = networkConfig // legacyParams;
        nodeConfig' = nodeConfig // legacyParams;
        NodeConfigFile' = "${__toFile "config-${cfg.environmentName}.json" (__toJSON nodeConfig')}";

      in lib.recursiveUpdate environments.${cfg.environmentName} {
        dbSyncConfig.NodeConfigFile = NodeConfigFile';
        explorerConfig.NodeConfigFile = NodeConfigFile';

        networkConfig = networkConfig';
        nodeConfig = nodeConfig';
      };
    in {
      additionalDbUsers = [
        "cardano-graphql"
        "smash"
        "cardano-rosetta-server"
        "dump-registered-relays-topology"
      ];

      environment = environmentConfig;
      explorerConfig = environmentConfig.dbSyncConfig;
    };

    services.varnish = {
      enable = true;
      package = opsPkgs.varnish70Packages.varnish;
      extraCommandLine = "-t ${toString (30 * 24 * 3600)} -s malloc,${toString (cfg.totalMachineMemoryGB * 1024 / 4)}M";
      config = ''
        vcl 4.1;
        import std;
        backend default {
          .host = "127.0.0.1";
          .port = "${toString config.services.smash.port}";
        }
        acl purge {
          "localhost";
          "127.0.0.1";
        }
        sub vcl_recv {
          unset req.http.x-cache;
          # Allow PURGE from localhost
          if (req.method == "PURGE") {
            if (!std.ip(req.http.X-Real-Ip, "0.0.0.0") ~ purge) {
              return(synth(405,"Not Allowed"));
            }
            # The host is included as part of the object hash
            # We need to match the public FQDN for the purge to be successful
            set req.http.host = "smash.${environmentConfig.domain}";
            return(purge);
          }
        }
        sub vcl_hit {
          set req.http.x-cache = "hit";
        }
        sub vcl_miss {
          set req.http.x-cache = "miss";
        }
        sub vcl_pass {
          set req.http.x-cache = "pass";
        }
        sub vcl_pipe {
          set req.http.x-cache = "pipe";
        }
        sub vcl_synth {
          set req.http.x-cache = "synth synth";
          set resp.http.x-cache = req.http.x-cache;
        }
        sub vcl_deliver {
          if (obj.uncacheable) {
            set req.http.x-cache = req.http.x-cache + " uncacheable";
          }
          else {
            set req.http.x-cache = req.http.x-cache + " cached";
          }
          set resp.http.x-cache = req.http.x-cache;
        }
        sub vcl_backend_response {
          if (bereq.uncacheable) {
            return (deliver);
          }
          if (beresp.status == 404) {
            set beresp.ttl = 1h;
          }
          call vcl_beresp_stale;
          call vcl_beresp_cookie;
          # Smash set "Cache-Control: no-store", so we skip this subroutine to still cache;
          # https://github.com/input-output-hk/cardano-db-sync/issues/1075
          # call vcl_beresp_control;
          call vcl_beresp_vary;
          return (deliver);
        }
      '';
    };

    services.cardano-node = {
      environments.${cfg.environmentName} = environmentConfig;
      nodeConfig = environmentConfig.nodeConfig;
    };

    services.cardano-ogmios = {
      enable = true;
      nodeConfig = cardanoNodeConfigPath;
      nodeSocket = nodeCfg.socketPath 0;
      hostAddr = "127.0.0.1";
    };

    services.graphql-engine = {
      enable = true;
      package = graphql-engine;
    };

    services.cardano-graphql = {
      enable = true;
      inherit cardanoNodeConfigPath;
      frontendPkg = cardano-graphql;
      persistPkg = persistgraphql;
      allowListPath = explorerAppPkgs.allowList;
      # allowListPath = lib.mkForce null;
      ogmiosHost = ogmiosCfg.hostAddr;
      ogmiosPort = ogmiosCfg.port;
      loggerMinSeverity = "trace";
    } // lib.optionalAttrs (options.services.cardano-graphql ? genesisByron) {
      genesisByron = nodeCfg.nodeConfig.ByronGenesisFile;
      genesisShelley = nodeCfg.nodeConfig.ShelleyGenesisFile;
    };

    services.cardano-graphql-background = {
      enable = true;
      frontendPkg = cardano-graphql;
      persistPkg = persistgraphql;
      hasuraCliPkg = hasura-cli;
      hasuraCliExtPkg = hasura-cli-ext;
      metadataServerUri = environmentConfig.metadataUrl or null;
      ogmiosHost = ogmiosCfg.hostAddr;
      ogmiosPort = ogmiosCfg.port;
      loggerMinSeverity = "debug";
    };

    services.cardano-rosetta-server = {
      enable = false;
      package = rosettaPkgs.cardano-rosetta-server;
      topologyFilePath = cardanoLibExplorer.mkEdgeTopology {
        edgeNodes = map (p: p.addr) nodeCfg.producers;
        port = nodeCfg.port;
      };
      cardanoCliPath = cardano-cli + /bin/cardano-cli;
      genesisPath = nodeCfg.nodeConfig.ShelleyGenesisFile;
      cardanoNodePath = cardano-node + /bin/cardano-node;
      cardanoNodeSocketPath = nodeCfg.socketPath 0;
      bindAddress = "127.0.0.1";
      port = 8082;
      dbConnectionString = "socket://${dbSyncCfg.postgres.user}:*@${dbSyncCfg.postgres.socketdir}?db=${dbSyncCfg.postgres.database}";
    };

    services.smash = {
      inherit dbSyncPkgs;
      enable = true;
      port = 3200;
      package = cardano-smash-server-no-basic-auth;

      environment = environmentConfig;
      environmentName = cfg.environmentName;
      explorerConfig = environmentConfig.dbSyncConfig;
      nodeConfig = environmentConfig.nodeConfig;
      logConfig = {};

      postgres = {inherit (dbSyncCfg.postgres) port database user socketdir;};
      delistedPools = auxConfig.${cfg.environmentName}.smashDelistedPools;
    };

    systemd.services.cardano-ogmios.serviceConfig = {
      DynamicUser = true;
      SupplementaryGroups = "cardano-node";
    };

    # Disable these extra systemd declarations as rosetta is set false for sunset
    # systemd.services.cardano-rosetta-server.serviceConfig = {
    #   DynamicUser = true;
    #   SupplementaryGroups = "cardano-node";
    # };

    systemd.services.cardano-graphql = {
      environment = {
        HOME = "/run/${config.systemd.services.cardano-graphql.serviceConfig.RuntimeDirectory}";
        POLLING_INTERVAL_ADA_SUPPLY = "3600000";
      };
      serviceConfig = {
        LimitNOFILE = 65535;
        RuntimeDirectory = "cardano-graphql";
        DynamicUser = true;
        Restart = "always";
        RestartSec = "5";
      };
    };

    # Required due to graphql-engine RuntimeMaxSec restarts for issue noted below
    systemd.services.cardano-graphql-background = {
      serviceConfig = {
        Restart = "always";
        RestartSec = "5";
      };
    };

    systemd.services.graphql-engine = {
      environment = {
        HASURA_GRAPHQL_LOG_LEVEL = "warn";
      };
      serviceConfig = {
        LimitNOFILE = 65535;
        Restart = "always";
        RestartSec = "5";
      };
    };

    systemd.services.cardano-submit-api.serviceConfig = lib.mkIf cfg.withSubmitApi {
      # Put cardano-submit-api in "cardano-node" group so that it can write socket file:
      SupplementaryGroups = "cardano-node";
      DynamicUser = true;
    };

    services.cardano-submit-api = lib.mkIf cfg.withSubmitApi {
      enable = true;
      port = 8101;
      environment = environmentConfig;
      config = environmentConfig.dbSyncConfig;
      socketPath = nodeCfg.socketPath 0;
      cardanoNodePackages = nodePkgs;
    };

    networking.firewall.extraCommands = ''
      # Allow scrapes for metrics to the private IP from the monitoring server
      iptables -A nixos-fw -s 192.168.254.100 -p tcp -m comment --comment "accept monitoring server traffic" -j nixos-fw-accept

      # Accept this environment cluster explorer gateway's requests over wireguard
      iptables -A nixos-fw -s 192.168.254.254/32 -p tcp --dport 80 -m comment --comment "upstream nginx proxypass" -j nixos-fw-accept
      iptables -A nixos-fw -s 192.168.254.254/32 -p tcp --dport 81 -m comment --comment "upstream nginx proxypass" -j nixos-fw-accept
      iptables -A nixos-fw -s 192.168.254.254/32 -p tcp --dport 9999 -m comment --comment "graphql-engine healthcheck" -j nixos-fw-accept
    '';

    systemd.services.explorer-topology-metrics-exporter = lib.mkIf config.services.nginx.enable {
      wantedBy = ["multi-user.target"];
      path = with pkgs; [coreutils netcat];
      script = ''
        IP="${wgIP}"
        PORT=8888
        FILE="/var/lib/registered-relays-dump/relays/topology.json"

        echo "Serving explorer topology metrics exporter for file $FILE at $IP:$PORT..."

        while true; do
          MTIME=$(date -r "$FILE" +%s || echo -n "0")
          BYTES=$(stat -c %s "$FILE" || echo -n "0")
          MTIME_DESC="# TYPE explorer_topology_mtime gauge"
          MTIME_SERIES="explorer_topology_mtime $MTIME"
          SIZE_DESC="# TYPE explorer_topology_bytes gauge"
          SIZE_SERIES="explorer_topology_bytes $BYTES"
          echo -e "HTTP/1.1 200 OK\r\nContent-Type: text/plain; version=0.0.4\r\n\r\n$MTIME_DESC\n$MTIME_SERIES\n$SIZE_DESC\n$SIZE_SERIES" | nc -W 1 -l "$IP" "$PORT"
          echo "$MTIME_SERIES"
          echo "$SIZE_SERIES"
        done
      '';

      serviceConfig = {
        Restart = "always";
        RestartSec = "30s";
      };
    };

    users.users.dump-registered-relays-topology = {
      isSystemUser = true;
      group = "dump-registered-relays-topology";
    };

    users.groups.dump-registered-relays-topology = {};

    systemd.services.dump-registered-relays-topology = lib.mkIf config.services.nginx.enable {
      wantedBy = ["multi-user.target"];
      after = ["network-online.target"];
      path = with pkgs; config.environment.systemPackages
        ++ [ config.services.postgresql.package jq netcat curl dnsutils ];
      environment = config.environment.variables;
      script = ''
        set -uo pipefail
        pingAddr() {
          index=$1
          addr=$2
          port=$3
          allAddresses=$(dig +nocookie +short -q "$addr" A || :)
          if [ -z "$allAddresses" ]; then
            allAddresses=$addr
          elif [ "$allAddresses" = ";; connection timed out; no servers could be reached" ]; then
            allAddresses=$addr
          fi
          while IFS= read -r ip; do
            set +e
            PING="$(timeout 7s cardano-cli ping -h "$ip" -p "$port" -m $NETWORK_MAGIC -c 1 -q --json)"
            res=$?
            if [ $res -eq 0 ]; then
              echo $PING | jq -c > /dev/null 2>&1
              res=$?
            fi
            set -e
            if [ $res -eq 0 ]; then
              >&2 echo "Successfully pinged $addr:$port (on ip: $ip)"
              set +e
              geoinfo=$(curl -s --retry 3 http://ip-api.com/json/$ip?fields=1105930)
              res=$?
              set -e
              if [ $res -eq 0 ]; then
                status=$(echo "$geoinfo" | jq -r '.status')
                if [ "$status" == "fail" ]; then
                  message=$(echo "$geoinfo" | jq -r '.message')
                  >&2 echo "Failed to retrieved goip info for $ip: $message"
                  exit 1
                fi
                continent=$(echo "$geoinfo" | jq -r '.continent')
                country_code=$(echo "$geoinfo" | jq -r '.countryCode')
                if [ "$country_code" == "US" ]; then
                  state=$(echo $geoinfo | jq -r '.regionName')
                  if [ "$state" == "Washington, D.C." ]; then
                    state="District of Columbia"
                  fi
                else
                  state=$country_code
                fi
                jq -c --arg addr "$addr" --arg port "$port" \
                  --arg continent "$continent" --arg state "$state" \
                  '{addr: $addr, port: $port|tonumber, continent: $continent, state: $state}' \
                  <<< '{}' \
                  > $index-relay.json
                break
              else
                >&2 echo "Failed to retrieved goip info for $ip"
                exit $res
              fi
            else
              >&2 echo "failed to cardano-ping $addr:$port (on ip: $ip)"
            fi
          done <<< "$allAddresses"
        }
        run() {
          epoch=$(cardano-cli query tip --testnet-magic $NETWORK_MAGIC | jq .epoch)
          db_sync_epoch=$(psql -U ${dbSyncCfg.postgres.user} -t --command="select no from epoch_sync_time order by id desc limit 1;")
          if [ $(( $epoch - $db_sync_epoch )) -gt 1 ]; then
            >&2 echo "cardano-db-sync has not catch-up with current epoch yet. Skipping."
            exit 0
          fi
          cd $STATE_DIRECTORY
          excludeList="$(sort relay_exclude.txt)"
          rm -f *-relay.json
          i=0
          for r in $(psql -U ${dbSyncCfg.postgres.user} -t < extract_relays.sql | jq -c '.[]'); do
            addr=$(echo "$r" | jq -r '.addr')
            port=$(echo "$r" | jq -r '.port')
            resolved=$(dig +nocookie +short -q "$addr" A || :)
            if [ "$resolved" = ";; connection timed out; no servers could be reached" ]; then
              sanitizedResolved=""
            else
              sanitizedResolved="$resolved"
            fi
            allAddresses=$addr$'\n'$sanitizedResolved
            excludedAddresses=$(comm -12 <(echo -e "$allAddresses" | sort) <(echo "$excludeList"))
            nbExcludedAddresses=$(echo $excludedAddresses | wc -w)
            if [[ $nbExcludedAddresses == 0 ]]; then
              ((i+=1))
              pingAddr $i "$addr" "$port" &
              sleep 1.5 # Due to rate limiting on ip-api.com
            else
              >&2 echo "$addr excluded due to dns name or IPs being in exclude list:\n$excludedAddresses"
            fi
          done
          wait
          if test -n "$(find . -maxdepth 1 -name '*-relay.json' -print -quit)"; then
            echo "Found a total of $(find . -name '*-relay.json' -printf '.' | wc -m) relays to include in topology.json"
            find . -name '*-relay.json' -printf '%f\t%p\n' | shuf | cut -d$'\t' -f2 | tr '\n' '\0' | xargs -r0 cat \
              | jq -n '. + [inputs]' | jq '{ Producers : . }' > topology.json
            mkdir -p relays
            mv topology.json relays/topology.json
            rm *-relay.json
          else
            echo "No relays found!!"
          fi
        }

        # Create relay_exclude.txt
        jq -r '.relaysExcludeList | .[]' < /etc/explorer/explorer.json > $STATE_DIRECTORY/relay_exclude.txt

        # Create extract_relays.sql
        if [ "$(jq -r '.poolsExcludeList | length' < /etc/explorer/explorer.json)" -gt 0 ]; then
          EXCLUDED_POOLS=$(jq -r ".poolsExcludeList | map_values(\"'\" + . + \"'\") | join(\", \")" < /etc/explorer/explorer.json)
          EXCLUDED_POOLS_SQL_MOD="pool_hash.view not in ($EXCLUDED_POOLS) and "
        else
          EXCLUDED_POOLS_SQL_MOD=""
        fi

        cat <<- EOF > $STATE_DIRECTORY/extract_relays.sql
          select array_to_json(array_agg(row_to_json(t))) from (
            select COALESCE(ipv4, dns_name) as addr, port from (
              select min(update_id) as update_id, ipv4, dns_name, port from pool_relay inner join pool_update
                ON pool_update.id = pool_relay.update_id inner join pool_hash ON pool_update.hash_id = pool_hash.id where $EXCLUDED_POOLS_SQL_MOD(
                (ipv4 is null and dns_name NOT LIKE '% %') or ipv4 !~ '(^0\.)|(^10\.)|(^100\.6[4-9]\.)|(^100\.[7-9]\d\.)|(^100\.1[0-1]\d\.)|(^100\.12[0-7]\.)|(^127\.)|(^169\.254\.)|(^172\.1[6-9]\.)|(^172\.2[0-9]\.)|(^172\.3[0-1]\.)|(^192\.0\.0\.)|(^192\.0\.2\.)|(^192\.88\.99\.)|(^192\.168\.)|(^198\.1[8-9]\.)|(^198\.51\.100\.)|(^203.0\.113\.)|(^22[4-9]\.)|(^23[0-9]\.)|(^24[0-9]\.)|(^25[0-5]\.)')
                group by ipv4, dns_name, port order by update_id
            ) t
          ) t;
        EOF

        while true
        do
          run
          sleep 3600
        done
      '';

      startLimitIntervalSec = 0;
      serviceConfig = {
        User = "dump-registered-relays-topology";
        # Need for cardano-cli:
        SupplementaryGroups = "cardano-node";
        StateDirectory = "registered-relays-dump";
        Restart = "always";
        RestartSec = "30s";
      };
    };

    services.nginx = {
      enable = true;
      enableReload = true;
      package = opsPkgs.nginxExplorer;
      eventsConfig = ''
        worker_connections 4096;
      '';
      appendConfig = ''
        worker_rlimit_nofile 16384;
      '';
      appendHttpConfig = ''
        vhost_traffic_status_zone;
        server {
          listen ${wgIP}:9113;
          location /status {
            vhost_traffic_status_display;
            vhost_traffic_status_display_format html;
          }
        }
      '';
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      commonHttpConfig = let
        luajit = opsPkgs.luajit;
        luaversion = luajit.lua.luaversion;
      in ''
        log_format x-fwd '$remote_addr - $remote_user [$time_local] '
                         '"$request" "$http_accept_language" $status $body_bytes_sent '
                         '"$http_referer" "$http_user_agent" "$http_x_forwarded_for"';
        access_log syslog:server=unix:/dev/log x-fwd;
        limit_req_zone $binary_remote_addr zone=apiPerIP:100m rate=1r/s;
        limit_req_status 429;
        map $http_accept_language $lang {
                default en;
                ~de de;
                ~ja ja;
        }
        map $arg_apiKey $api_client_name {
          default "";
          SED_MARKER_SMASH_API_KEYS
        }
        map $http_origin $origin_allowed {
          default 0;
          SED_MARKER_SMASH_ALLOWED_ORIGINS
        }
        map $sent_http_x_cache $loggable_varnish {
          "hit cached" 0;
          default 1;
        }
        map $origin_allowed $origin {
          default "";
          1 $http_origin;
        }
        # set search paths for pure Lua external libraries (';;' is the default path):
        lua_package_path '${luajit}/share/lua/${luaversion}/?.lua;${luajit}/lib/lua/${luaversion}/?.lua;;';
        # set search paths for Lua external libraries written in C (can also use ';;'):
        lua_package_cpath '${luajit}/lib/lua/${luaversion}/?.so;${luajit}/share/lua/${luaversion}/?.so;;';
        init_by_lua_block {
          json = require "cjson"
        }
      '';
      virtualHosts = {
        explorer = {
          default = true;
          locations = (if maintenanceMode then {
            "/" = let
              maintenanceFile = __toFile "maintenance.html" ''
                <!doctype html>
                <title>Site Maintenance</title>
                <style>
                  body { text-align: center; padding: 150px; }
                  h1 { font-size: 50px; }
                  body { font: 20px Helvetica, sans-serif; color: #333; }
                  article { display: block; text-align: left; width: 650px; margin: 0 auto; }
                  a { color: #dc8100; text-decoration: none; }
                  a:hover { color: #333; text-decoration: none; }
                </style>
                <article>
                    <h1>We&rsquo;ll be back soon!</h1>
                    <div>
                        <p>Sorry for the inconvenience, but we&rsquo;re performing some routine maintenance on the explorer at the moment. We&rsquo;ll be back online shortly!</p>
                        <p>&mdash; IOHK DevOps</p>
                    </div>
                </article>
              '';
              rootDir = pkgs.runCommand "nginx-root-dir" {} ''
                mkdir $out
                cd $out
                cp ${maintenanceFile} index.html;
              '';
            in {
              extraConfig = ''
                etag off;
                add_header etag "\"${builtins.substring 11 32 rootDir}\"";
                root ${rootDir};
              '';
              tryFiles = "$uri /index.html";
            };
          } else let
            graphqlRewriter = query: postProcessing: ''
              rewrite_by_lua_block {
                  ngx.req.read_body()
                  ngx.req.set_header("Content-Type", "application/json")
                  ngx.req.set_method(ngx.HTTP_POST)
                  ngx.req.set_uri("/graphql")
                  ngx.req.set_body_data("{\"query\":\"${query}\"}")
                }
                header_filter_by_lua_block {
                  ngx.header.content_length = nil
                }
                body_filter_by_lua_block {
                  local chunk, eof = ngx.arg[1], ngx.arg[2]
                  local buf = ngx.ctx.buf
                  if eof then
                    if buf then
                      local obj = json.decode(buf .. chunk)
                      ngx.arg[1] = ${postProcessing}
                    end
                  else
                    if buf then
                      ngx.ctx.buf = buf .. chunk
                    else
                      ngx.ctx.buf = chunk
                    end
                    ngx.arg[1] = nil
                  end
                }
              '';
            rosettaHealth = ''
              rewrite_by_lua_block {
                  ngx.req.read_body()
                  ngx.req.set_header("Content-Type", "application/json")
                  ngx.req.set_method(ngx.HTTP_POST)
                  ngx.req.set_uri("/network/list")
                  ngx.req.set_body_data("{\"metadata\":{}}")
                }
            '';
          in {
            "/" = {
              root = (explorerAppPkgs.overrideScope'(self: super: {
                static = super.static.override {
                  # For testing with ssh tunnel and no frontend gateway
                  # graphqlApiProtocol = "http";
                  # graphqlApiPort = 80;

                  graphqlApiHost = cfg.explorerHostName;
                  cardanoNetwork = cfg.environmentName;
                  gaTrackingId = null;
                };
              })).static;
              tryFiles = "$uri $uri/index.html /index.html";
              extraConfig = ''
                rewrite /tx/([0-9a-f]+) $http_x_forwarded_proto://$host/$lang/transaction.html?id=$1 redirect;
                rewrite /address/([0-9a-zA-Z]+) $http_x_forwarded_proto://$host/$lang/address.html?address=$1 redirect;
                rewrite /block/([0-9a-zA-Z]+) $http_x_forwarded_proto://$host/$lang/block.html?id=$1 redirect;
                rewrite /epoch/([0-9]+) $http_x_forwarded_proto://$host/$lang/epoch.html?number=$1 redirect;
                rewrite ^([^.]*[^/])$ $http_x_forwarded_proto://$host$1.html redirect;
              '';
            };
            # To avoid 502 alerts when withSubmitApi is false
            "/api/submit/tx" = lib.mkIf cfg.withSubmitApi {
              proxyPass = "http://127.0.0.1:8101/api/submit/tx";
            };
            "/graphql" = {
              proxyPass = "http://127.0.0.1:3100/";
            };
            "/rosetta/" = {
              proxyPass = "http://127.0.0.1:8082/";
            };
            "/rosetta/health" = {
              proxyPass = "http://127.0.0.1:8082/";
              extraConfig = rosettaHealth;
            };
            "/supply/total" = {
              proxyPass = "http://127.0.0.1:3100/";
              extraConfig = graphqlRewriter
                "{ ada { supply { total } } }"
                "obj.data.ada.supply.total / 1000000";
            };
            "/supply/circulating" = {
              proxyPass = "http://127.0.0.1:3100/";
              extraConfig = graphqlRewriter
                "{ ada { supply { circulating } } }"
                "obj.data.ada.supply.circulating / 1000000";
            };
          }) // {
            "/relays" = {
              root = "/var/lib/registered-relays-dump";
            };
            "/metrics2/cardano-graphql" = {
              proxyPass = "http://127.0.0.1:3100/metrics";
            };
          };
        };
      } // (lib.optionalAttrs cfg.withSmash {
        smash = {
          listen = [ {
            addr = "0.0.0.0";
            port = 81;
          }];
          default = true;
          locations =
            let
            apiKeyConfig = ''
              if ($arg_apiKey = "") {
                  return 401; # Unauthorized (please authenticate)
              }
              if ($api_client_name = "") {
                  return 403; # Forbidden (invalid API key)
              }
            '';
            corsConfig = ''
              add_header 'Vary' 'Origin' always;
              add_header 'Access-Control-Allow-Origin' $origin always;
              add_header 'Access-Control-Allow-Methods' 'GET, PATCH, OPTIONS' always;
              add_header 'Access-Control-Allow-Headers' 'User-Agent,X-Requested-With,Content-Type' always;
              if ($request_method = OPTIONS) {
                add_header 'Access-Control-Max-Age' 1728000;
                add_header 'Content-Type' 'text/plain; charset=utf-8';
                add_header 'Content-Length' 0;
                return 204;
              }
            '';
            endpoints = [
              "/swagger.json"
              "/api/v1/metadata"
              "/api/v1/errors"
              "/api/v1/exists"
              "/api/v1/enlist"
              "/api/v1/delist"
              "/api/v1/delisted"
              "/api/v1/retired"
              "/api/v1/status"
              "/api/v1/tickers"
            ];
            in lib.recursiveUpdate (lib.genAttrs endpoints (p: {
              proxyPass = "http://127.0.0.1:6081${p}";
              extraConfig = corsConfig;
            })) {
              "/api/v1/delist".extraConfig = ''
                ${corsConfig}
                ${apiKeyConfig}
              '';
              "/api/v1/enlist".extraConfig = ''
                ${corsConfig}
                ${apiKeyConfig}
              '';
              "/api/v1/metadata".extraConfig = ''
                ${corsConfig}
              '';
              "/api/v1/tickers".extraConfig = ''
                ${corsConfig}
                if ($request_method = GET) {
                  set $arg_apiKey "bypass";
                  set $api_client_name "bypass";
                }
                ${apiKeyConfig}
              '';
            };
        };
      });
    };

    # To enable the transformation of sops nginx secrets into nginx config.
    # Lua substitution approach doesn't work due to substitution stanza location.
    # Envsubst doesn't work due to common use of nginx variables greedily substituted.
    # Nginx enableReload is set true above to access the base config for susbstitution here.
    systemd.services.nginx = let
      script = pkgs.writeShellApplication {
        name = "nginx-prep";
        runtimeInputs = with pkgs; [gnused jq nginx-config-formatter];
        text = ''
          set -x
          cd /var/lib/nginx
          cp /etc/nginx/nginx.conf nginx.conf
          chmod +w nginx.conf
          jq -r '.smashApiKeys | to_entries[] | "\"\(.value)\" \"\(.key)\";"' < /etc/explorer/explorer.json > smash-api-keys
          jq -r '.smashAllowedOrigins | .[] | . + " 1;"' < /etc/explorer/explorer.json > smash-allowed-origins
          sed -e '/SED_MARKER_SMASH_API_KEYS/ {' -e 'r smash-api-keys' -e 'd' -e '}' nginx.conf > nginx-subst-partial.conf
          sed -e '/SED_MARKER_SMASH_ALLOWED_ORIGINS/ {' -e 'r smash-allowed-origins' -e 'd' -e '}' nginx-subst-partial.conf > nginx-subst.conf
          nginxfmt nginx.conf
          nginxfmt nginx-subst.conf
          rm nginx-subst-partial.conf

          ${opsPkgs.nginxExplorer}/bin/nginx -c /var/lib/nginx/nginx-subst.conf -t
        '';
      };
    in {
      preStart = lib.mkForce "${script}/bin/nginx-prep";
      serviceConfig = {
        # Ensure the worker processes don't hit TCP file descriptor limits
        LimitNOFILE = 65535;

        # Avoid flooding (and rotating too quicky) default journal with nginx logs:
        # nginx logs: journalctl --namespace nginx
        LogNamespace = "nginx";

        # Access to topology.json:
        SupplementaryGroups = "dump-registered-relays-topology";

        StateDirectory = "nginx";
        ExecStart = lib.mkForce "${opsPkgs.nginxExplorer}/bin/nginx -c /var/lib/nginx/nginx-subst.conf";
        ExecReload = lib.mkForce [
          "${script}/bin/nginx-prep"
          "${pkgs.coreutils}/bin/kill -HUP $MAINPID"
        ];
      };
    };

    services.prometheus.exporters = {
      # Firewall handling is done in the networking.firewall.extraCommands block above
      node = {
        enable = true;
        listenAddress = wgIP;
        port = 9100;
        enabledCollectors = [
          "bonding"
          "conntrack"
          "cpu"
          "cpufreq"
          "diskstats"
          "edac"
          "entropy"
          "filefd"
          "filesystem"
          "ksmd"
          "loadavg"
          "logind"
          "meminfo"
          "netdev"
          "netstat"
          "processes"
          "pressure"
          "nvme"
          "sockstat"
          "stat"
          "systemd"
          "tcpstat"
          "time"
          "timex"
          "vmstat"
        ];
      };

      # Nginx exporter is already enabled directly with the nginx vts module provided in appendHttpConfig above

      varnish = {
        enable = true;
        listenAddress = wgIP;
        port = 9131;
        group = "varnish";
        instance = "/var/spool/varnish/${name}";
      };

      wireguard = {
        enable = true;
        listenAddress = wgIP;
        port = 9586;
      };
    };

    services.telegraf.extraConfig.outputs.influxdb.urls = ["http://192.168.254.100:8428"];

    secrets.install.explorer = lib.mkIf isSops {
      source = "${etcEncrypted}/explorer.json";
      target = "/etc/explorer/explorer.json";
    };
  };
}
