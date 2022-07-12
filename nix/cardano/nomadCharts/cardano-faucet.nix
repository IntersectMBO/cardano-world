{
  inputs,
  cell,
}: let
  inherit (inputs) data-merge cells nixpkgs;
  inherit (inputs.bitte-cells) vector _utils;
  inherit (cell) healthChecks constants oci-images;
  l = nixpkgs.lib // builtins;
  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";
in
  {
    jobname ? "cardano-faucet",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
    domain,
    nodeClass,
    scaling,
  } @ args: let
    id = jobname;
    type = "service";
    priority = 50;
    vaultPkiPath = "pki/issue/cardano-faucet";
    consulRolePath = "consul/creds/cardano-faucet";
  in
    with data-merge; {
      job.${id} = {
        inherit namespace datacenters id type priority;
        # ----------
        # Scheduling
        # ----------
        constraint = [
          {
            attribute = "\${node.class}";
            operator = "=";
            value = "${nodeClass}";
          }
          # {
          #   attribute = "\${meta.cardano}";
          #   operator = "is_set";
          # }
          {
            operator = "distinct_hosts";
            value = "true";
          }
        ];
        spread = [{attribute = "\${node.datacenter}";}];
        # ----------
        # Update
        # ----------
        update.health_check = "task_states";
        update.healthy_deadline = "5m0s";
        update.max_parallel = 1;
        update.min_healthy_time = "10s";
        update.progress_deadline = "60m0s";
        update.stagger = "30s";
        # ----------
        # Migrate
        # ----------
        migrate.health_check = "checks";
        migrate.healthy_deadline = "8m20s";
        migrate.max_parallel = 1;
        migrate.min_healthy_time = "10s";
        # ----------
        # Reschedule
        # ----------
        reschedule.delay = "30s";
        reschedule.delay_function = "exponential";
        reschedule.max_delay = "1h0m0s";
        reschedule.unlimited = true;
        # ----------
        # Task Groups
        # ----------
        group.cardano-faucet = let
          # work-around: we need to get rid of vector first
          node' = (cell.nomadCharts.cardano-node (args // {jobname = "node";})).job.node.group.cardano;
          group = l.removeAttrs node' ["task"];
          node = group // {task.node = node'.task.node;};
        in
          merge
          # task.vector ...
          (vector.nomadTask.default {
            inherit namespace;
            endpoints = [
              # prometheus metrics for cardano-faucet
              "http://127.0.0.1:8090/metrics"
            ];
          })
          (
            merge node
            {
              count = scaling;
              # service = append [
              #   (import ./srv-cardano-faucet.nix {inherit namespace healthChecks;})
              # ];
              network.port.http.to = "8090";
              service = append [
                {
                  name = "cardano-faucet";
                  port = "http";
                  tags = [
                    "ingress"
                    "traefik.enable=true"
                    "traefik.http.routers.cardano-faucet.rule=Host(`faucet.${domain}`) && PathPrefix(`/send-money`)"
                    "traefik.http.routers.cardano-faucet.entrypoints=https"
                    "traefik.http.routers.cardano-faucet.tls.certresolver=acme"
                  ];
                  check = [
                    {
                      type = "tcp";
                      port = "http";
                      interval = "10s";
                      timeout = "2s";
                    }
                  ];
                }
              ];

              task = {
                # ----------
                # Task: Cardano-Faucet
                # ----------
                cardano-faucet = {
                  env.CARDANO_NODE_SOCKET_PATH = "/alloc/tmp/node.socket"; # figure out how to pass this from the cardano group
                  env.CONFIG_FILE = "/secrets/faucet-config.json";
                  env.PORT = "8090";

                  template =
                    _utils.nomadFragments.workload-identity-vault {inherit vaultPkiPath;}
                    ++ [
                      {
                        change_mode = "restart";
                        data = ''
                          {{- with secret "kv/data/faucet/${namespace}" }}{{ .Data.data | toJSONPretty }}{{ end -}}
                        '';
                        destination = "/secrets/faucet-config.json";
                      }
                    ];

                  env.WORKLOAD_CACERT = "/secrets/tls/ca.pem";
                  env.WORKLOAD_CLIENT_KEY = "/secrets/tls/key.pem";
                  env.WORKLOAD_CLIENT_CERT = "/secrets/tls/cert.pem";
                  config.image = ociNamer oci-images.cardano-faucet;
                  config.ports = ["http"];
                  user = "0:0";
                  driver = "docker";
                  kill_signal = "SIGINT";
                  kill_timeout = "30s";
                  resources.cpu = 2000;
                  resources.memory = 4096;
                  vault = {
                    change_mode = "noop";
                    env = true;
                    policies = ["cardano-faucet"];
                  };
                };
              };
            }
          );
      };
    }
