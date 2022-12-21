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
}
