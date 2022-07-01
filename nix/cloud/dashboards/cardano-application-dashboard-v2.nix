{ lib, ... }:
(((lib.dashboard.new {
  title = "Cardano: Application metrics v2";
  timezone= "utc";
  uid= "Oe0reiHef";
  schemaVersion= 34;
  graphTooltip = 0;
})
  .addAnnotation
    (lib.kxPop (lib.annotation.datasource {
      inherit (lib.annotation.default)
        builtIn
        datasource
        hide
        iconColor
        name
        type;
    }) {
      "$$hashKey" = "object:345";
      target = {
        limit = 100;
        matchAny = false;
        tags = [ ];
        type = "dashboard";
      };
    })
  )
  .addPanel {
    panel = ((((lib.statPanel.new {
      title= "Current Node Version";
      pluginVersion= "8.3.4";
      datasource = {
        type = "prometheus";
        uid= "P1809F7CD0C75ACF3";
      };
      "colorMode"= "value";
      "graphMode"= "none";
      "justifyMode"= "auto";
      "orientation"= "horizontal";
      reducerFunction = "last";
    })
      .addMapping {
        "options"= {
          "match"= "null";
          "result"= {
            "text"= "N/A";
          };
        };
        "type"= "special";
      })
      .addThreshold {
        color = "green";
        value = null;
      })
      .addTargets [
        (lib.prometheus.target {
          "expr"= "min(cardano_node_cli_version_major{alias!~\"snapshots|explorer-.*|.*Test.*\"})";
          "instant"= true;
          "interval"= "";
          "legendFormat"= "Node Major Version:";
        })
        (lib.prometheus.target {
          "expr"= "min(cardano_node_cli_version_major{alias!~\"snapshots|explorer-.*|.*Test.*\"})";
          "instant"= true;
          "interval"= "";
          "legendFormat"= "Node Major Version:";
          "refId"= "A";
        })
        (lib.prometheus.target {
          "expr"= "min(cardano_node_cli_version_minor{alias!~\"snapshots|explorer-.*|.*Test.*\"})";
          "instant"= true;
          "interval"= "";
          "legendFormat"= "Node Minor Version:";
          "refId"= "B";
        })
        (lib.prometheus.target {
          "expr"= "min(cardano_node_cli_version_patch{alias!~\"snapshots|explorer-.*|.*Test.*\"})";
          "instant"= true;
          "interval"= "";
          "legendFormat"= "Node Patch Version:";
          "refId"= "C";
        })
      ])
      ;
    gridPos = {
      "h"= 6;
      "w"= 8;
      "x"= 0;
      "y"= 0;
    };
  })
