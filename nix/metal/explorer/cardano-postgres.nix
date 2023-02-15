{ pkgs, config, lib, ... }:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.services.cardano-postgres;
in {
  options.services.cardano-postgres.enable = mkEnableOption "Cardano Postgres";

  config = mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql_12;
      enableTCPIP = false;
      settings = {
        # Optimized for:
        # DB Version: 12
        # OS Type: linux
        # DB Type: mixed (DW and OLTP characteristics)
        # Total Memory (RAM): 112 GB
        #     256 GB m3.large.x86
        #   - 64 graphql-engine
        #   - 24 db-sync
        #   - 20 node
        #   - 20 zfs arc
        #   - 16 misc
        # CPUs num: 50
        #     64 vCPU m3.large.x86
        #   - 2 graphql-engine
        #   - 2 db-sync
        #   - 2 node
        #   - 8 misc
        # Connections num: 2000
        # Data Storage: ssd
        # Suggested optimization for
        # other configurations can be
        # found at:
        # https://pgtune.leopard.in.ua/
        max_connections = 2000;
        shared_buffers = "28GB";
        effective_cache_size = "84GB";
        maintenance_work_mem = "2GB";
        checkpoint_completion_target = 0.9;
        wal_buffers = "16MB";
        default_statistics_target = 100;
        random_page_cost = 1.1;
        effective_io_concurrency = 200;
        work_mem = "1835kB";
        min_wal_size = "1GB";
        max_wal_size = "4GB";
        max_worker_processes = 50;
        max_parallel_workers_per_gather = 4;
        max_parallel_workers = 50;
        max_parallel_maintenance_workers = 4;
        # To reduce impact of vacuum on iowait (default is 2)
        autovacuum_vacuum_cost_delay = 20;

        # Uncomment for query analysis
        # During non-IO bound runtime, may impact performance up to ~10%
        shared_preload_libraries = "pg_stat_statements";
        "pg_stat_statements.track" = "all";
      };
    };
  };
}
