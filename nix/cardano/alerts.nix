{
  inputs,
  cell,
}:
let
  chainDensityLow = toString 70;
  highBlockUtilization = toString 95; # Alert if blocks are above that % full.

in
{

  node = {
    datasource = "vm";
    rules =
      [
        {
          alert = "blackbox_probe_down";
          expr = "probe_success == 0";
          for = "5m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.job}}: Blackbox probe is down for {{$labels.instance}}.";
            description = "{{$labels.job}}: Blackbox probe has been down for at least 5 minutes for {{$labels.instance}}.";
          };
        }
        {
          alert = "high_cardano_ping_latency";
          expr = "avg_over_time(cardano_ping_latency_ms[5m]) > 250";
          for = "30m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: Cardano average ping latency over 5 minutes has been above 250 milliseconds for the last 30 minutes";
            description = "{{$labels.nomad_alloc_name}}: Cardano average ping latency over 5 minutes has been above 250 milliseconds for the last 30 minutes.";
          };
        }
        {
          alert = "chain_quality_degraded";
          expr = "100 * quantile by(namespace) (0.2, (cardano_node_metrics_density_real * 20)) < ${chainDensityLow}";
          for = "5m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "Degraded Chain Density: more than 20% of nodes have low chain density (<${chainDensityLow}%) in namespace {{$labels.namespace}}.";
            description = "Degraded Chain Density: more than 20% of nodes have low chain density (<${chainDensityLow}%) in namespace {{$labels.namespace}}.";
          };
        }
        # {
        #   alert = "blocks_adoption_delay_too_high";
        #   expr = "avg(quantile_over_time(0.95, cardano_node_metrics_blockadoption_forgeDelay_real[6h])) >= 4.5";
        #   for = "1m";
        #   labels = {
        #     severity = "page";
        #   };
        #   annotations = {
        #     summary = "Blocks adoption delay have been above 4.5s for more than 5% of blocks";
        #     description = "Node average of blocks adoption delay have been above 4.5s for more than 5% of blocks for more than 6 hours";
        #   };
        # }
        # {
        #   alert = "blocks_utilization_too_high";
        #   expr = "100 * avg(avg_over_time(cardano_node_metrics_blockfetchclient_blocksize[6h]) / on(alias) (cardano_node_protocol_maxBlockBodySize + cardano_node_protocol_maxBlockHeaderSize)) > ${highBlockUtilization}";
        #   for = "5m";
        #   labels = {
        #     severity = "page";
        #   };
        #   annotations = {
        #     summary = "Blocks utilization above ${highBlockUtilization}% - follow process in description.";
        #     description = "Blocks utilization has been above ${highBlockUtilization}% on average for more than 6h. Follow process at https://docs.google.com/document/d/1H42XpVp5YKUfKTcfyV_YJP5nM2N5D9eU_0MvFbXXp0E";
        #   };
        # }
        # {
        #   alert = "cardano_new_node_block_divergence";
        #   expr = "((abs(max(cardano_node_metrics_blockNum_int) - ignoring(alias, instance, job, role) group_right(instance) cardano_node_metrics_blockNum_int) > bool 2) - (abs(max(cardano_node_metrics_slotNum_int) - ignoring(alias, instance, job, role) group_right(instance) cardano_node_metrics_slotNum_int) < bool 60)) == 1";
        #   for = "5m";
        #   labels = {
        #     severity = "page";
        #   };
        #   annotations = {
        #     summary = "{{$labels.nomad_alloc_name}}: cardano-node block divergence detected for more than 5 minutes in namespace {{$labels.namespace}}";
        #     description = "{{$labels.nomad_alloc_name}}: cardano-node block divergence of more than 2 blocks and 60 seconds lag detected for more than 5 minutes in namespace {{$labels.namespace}}";
        #   };
        # }
        {
          alert = "cardano_new_node_blockheight_unchanged";
          expr = "rate(cardano_node_metrics_blockNum_int[1m]) == 0";
          for = "10m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: cardano-node blockheight unchanged for more than 10 minutes in namespace {{$labels.namespace}}.";
            description = "{{$labels.nomad_alloc_name}}: cardano-node blockheight unchanged for more than 10 minutes at a 1 minute rate resolution in namespace {{$labels.namespace}}.";
          };
        }
        {
          alert = "cardano_new_node_forge_not_adopted_error";
          expr = "increase(cardano_node_metrics_Forge_didnt_adopt_int[1h]) > 5";
          for = "1m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: cardano-node is failing to adopt a significant amount of recent forged blocks in namespace {{$labels.namespace}}";
            description = ''
              {{$labels.nomad_alloc_name}}: cardano-node failed to adopt more than 5 forged blocks in the past hour in namespace {{$labels.namespace}}.
              A restart of node on the affected machine(s) may be required.'';
          };
        }
        {
          alert = "too_many_slot_leadership_checks_missed";
          expr = "rate(cardano_node_metrics_slotsMissedNum_int[5m]) * 1 > 0.5";
          for = "2m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: block producing node is failing to check for slot leadership for more than half of the slots in namespace {{$labels.namespace}}.";
            description = "{{$labels.nomad_alloc_name}}: block producing node is failing to check for slot leadership for more than half of the slots for more than 2 min in namespace {{$labels.namespace}}.";
          };
        }
        {
          alert = "cardano_new_node_KES_expiration_metric_10period_notice";
          expr = "cardano_node_metrics_remainingKESPeriods_int <= 10";
          for = "5m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: cardano-node KES expiration notice: less than 10 periods until KES expiration in namespace {{$labels.namespace}}";
            description = "{{$labels.nomad_alloc_name}}: cardano-node KES expiration notice: less than 10 periods until KES expiration; calculated from node metrics in namespace {{$labels.namespace}}";
          };
        }
        {
          alert = "cardano_new_node_KES_expiration_metric_5period_notice";
          expr = "cardano_node_metrics_remainingKESPeriods_int <= 5";
          for = "5m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: cardano-node KES expiration notice: less than 5 periods until KES expiration in namespace {{$labels.namespace}}";
            description = "{{$labels.nomad_alloc_name}}: cardano-node KES expiration notice: less than 5 periods until KES expiration; calculated from node metrics in namespace {{$labels.namespace}}";
          };
        }
        {
          alert = "cardano_new_node_KES_expiration_metric_1period_warning";
          expr = "cardano_node_metrics_remainingKESPeriods_int <= 1";
          for = "5m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "{{$labels.nomad_alloc_name}}: cardano-node KES expiration warning: less than 1 periods until KES expiration in namespace {{$labels.namespace}}";
            description = "{{$labels.nomad_alloc_name}}: cardano-node KES expiration warning: less than 1 periods until KES expiration; calculated from node metrics in namespace {{$labels.namespace}}";
          };
        }
      ];
  };

  dbsync = {
    datasource = "vm";
    rules =
      [
        {
          alert = "dbsync_db_block_height_stall";
          expr = "increase(cardano_db_sync_db_block_height[1m]) == 0";
          for = "30m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "Dbsync job {{$labels.nomad_alloc_index}} in namespace {{$labels.namespace}} is experiencing block height stall.";
            description = "Dbsync job {{$labels.nomad_alloc_index}} in namespace {{$labels.namespace}} has not increased in DB block height for the past 30 minutes";
          };
        }
        {
          alert = "dbsync_node_block_height_stall";
          expr = "increase(cardano_db_sync_node_block_height[1m]) == 0";
          for = "10m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "Dbsync job {{$labels.nomad_alloc_index}} in namespace {{$labels.namespace}} is experiencing cardano node block height stall.";
            description = "Dbsync job {{$labels.nomad_alloc_index}} in namespace {{$labels.namespace}} has not observed cardano node block height for the past 10 minutes";
          };
        }
        {
          alert = "dbsync_node_block_height_divergence";
          expr = "abs(cardano_db_sync_node_block_height - cardano_db_sync_db_block_height) > 10";
          for = "30m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Dbsync job {{$labels.nomad_alloc_index}} is experiencing block height divergence from cardano node.";
            description = ''
              Dbsync job {{$labels.nomad_alloc_index}} in namespace {{$labels.namespace}} has averaged more than 10 blocks divergence with node for more than 10 minutes.
              During extended resynchronization events this may be expected and should resolve once synchronization is complete.'';
          };
        }
        {
          alert = "dbsync_queue_length";
          expr = "cardano_db_sync_db_queue_length > 10";
          for = "30m";
          labels = {
            severity = "warning";
          };
          annotations = {
            summary = "Dbsync job {{$labels.nomad_alloc_index}} is experiencing queue backlog.";
            description = ''
              Dbsync job {{$labels.nomad_alloc_index}} in namespace {{$labels.namespace}} has queue length of > 10 for more than 30 minutes.
              During extended resynchronization events this may be expected and should resolve once synchronization is complete.'';
          };
        }
      ];
  };

  faucet = {
    datasource = "vm";
    rules =
      [
        {
          alert = "faucet_utxo_low";
          expr = ''faucet_utxo{is_valid="1"} < 2000'';
          for = "1m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "Faucet UTxO is low in namespace {{$labels.namespace}}.";
            description = ''
              Faucet has less than 2000 UTxO remaining in namespace {{$labels.namespace}} on alloc {{$labels.nomad_alloc_name}}.'';
          };
        }
        {
          alert = "faucet_utxo_empty";
          expr = ''faucet_utxo{is_valid="1"} == 0'';
          for = "1m";
          labels = {
            severity = "page";
          };
          annotations = {
            summary = "Faucet UTxO is empty in namespace {{$labels.namespace}}.";
            description = ''
              Faucet has no available UTxO in namespace {{$labels.namespace}} on alloc {{$labels.nomad_alloc_name}}.'';
          };
        }
    ];
  };

  metal-explorer = {
    datasource = "vm";
    rules = [
      {
        alert = "node_down";
        expr = ''up == 0'';
        for = "5m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: Node is down.";
          description = "{{$labels.alias}} has been down for more than 5 minutes.";
        };
      }
      {
        alert = "node_systemd_service_failed";
        expr = ''node_systemd_unit_state{state="failed"} == 1'';
        for = "5m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: Service {{$labels.name}} failed to start.";
          description = "{{$labels.alias}} failed to (re)start service {{$labels.name}}.";
        };
      }
      {
        alert = "node_filesystem_full_90percent";
        expr = ''sort(node_filesystem_free_bytes{device!="ramfs"} < node_filesystem_size_bytes{device!="ramfs"} * 0.1) / 1024^3'';
        for = "5m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: Filesystem is running out of space soon.";
          description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} got less than 10% space left on its filesystem.";
        };
      }
      {
        alert = "node_filesystem_full_in_4h";
        expr = ''predict_linear(node_filesystem_free_bytes{device!~"ramfs|tmpfs|none",fstype!~"autofs|ramfs|cd9660"}[4h], 4*3600) <= 0'';
        for = "5m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: Filesystem is running out of space in 4 hours.";
          description = "{{$labels.alias}} device {{$labels.device}} on {{$labels.mountpoint}} is running out of space of in approx. 4 hours";
        };
      }
      {
        alert = "node_filedescriptors_full_in_3h";
        expr = ''predict_linear(node_filefd_allocated[1h], 3*3600) >= node_filefd_maximum'';
        for = "20m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}} is running out of available file descriptors in 3 hours.";
          description = "{{$labels.alias}} is running out of available file descriptors in approx. 3 hours";
        };
      }
      {
        alert = "node_time_unsync";
        expr = ''abs(node_timex_estimated_error_seconds) > 0.500'';
        for = "5m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: Clock out of sync with NTP";
          description = "{{$labels.alias}} Local clock offset is too large or out of sync with NTP";
        };
      }
      {
        alert = "http_high_internal_error_rate";
        expr = ''rate(nginx_vts_server_requests_total{code="5xx"}[5m]) * 50 > on(alias, host) rate(nginx_vts_server_requests_total{code="2xx"}[5m])'';
        for = "15m";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: High http internal error (code 5xx) rate";
          description = "{{$labels.alias}}  number of correctly served requests is less than 50 times the number of requests aborted due to an internal server error";
        };
      }
      {
        alert = "varnish cache too small or ttl too long";
        expr = ''rate(varnish_main_n_lru_nuked[30m]) > 5 * rate(varnish_main_n_expired[30m])'';
        for = "1h";
        labels.severity = "page";
        annotations = {
          summary = "{{$labels.alias}}: Too many objects (5 times the number of expiring objects) are being forcefully evicted from varnish cache due to memory constraints.";
          description = "{{$labels.alias}}: Consider increasing varnish malloc limit or decreasing beresp.ttl";
        };
      }
    ];
  };
}
