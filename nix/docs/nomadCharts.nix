{
  inputs,
  cell,
}: let
  inherit (cell) oci-images;
  # OCI-Image Namer
  ociNamer = oci: builtins.unsafeDiscardStringContext "${oci.imageName}:${oci.imageTag}";
in {
  documentation = {
    jobname ? "documentation",
    namespace,
    datacenters ? ["eu-central-1" "eu-west-1" "us-east-2"],
    domain,
    nodeClass,
    scaling,
  }: let
    id = jobname;
    type = "service";
    priority = 50;
  in {
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
      ];
      spread = [
        {
          attribute = "\${node.datacenter}";
          weight = "100";
        }
      ];
      # ----------
      # Update
      # ----------
      update.health_check = "task_states";
      update.healthy_deadline = "5m0s";
      update.max_parallel = 1;
      update.min_healthy_time = "10s";
      update.progress_deadline = "10m0s";
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
      group.public = {
        count = scaling;
        network = {
          dns = {servers = ["172.17.0.1"];};
          mode = "bridge";
          port.http = {to = 8080;};
        };
        service = [
          {
            name = "public-documentation";
            port = "http";
            tags = [
              "ingress"
              "traefik.enable=true"
              "traefik.http.routers.public-documentation.rule=Host(`book.world.dev.cardano.org`) && PathPrefix(`/`)"
              "traefik.http.routers.public-documentation.entrypoints=https"
              "traefik.http.routers.public-documentation.tls=true"
              "traefik.http.routers.public-documentation.tls.certresolver=acme"
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
        task.static-page = {
          config.image = ociNamer oci-images.public-documentation;
          driver = "docker";
        };
      };
    };
  };
}
