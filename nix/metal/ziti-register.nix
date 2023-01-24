{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) mkIf;
in {
  # Host the Ziti Admin Console ("zac") via routing, with traefik providing TLS certs
  systemd.services.ziti-console-service =
    mkIf config.services.ziti-console.enable
    (pkgs.consulRegister {
      pkiFiles.caCertFile = "/etc/ssl/certs/ca.pem";
      service = {
        name = "ziti-console";
        port = config.services.ziti-console.portHttp;
        tags = [
          "ziti"
          "ziti-console"
          "zac.${config.cluster.domain}"
          "ingress"
          "traefik.enable=true"
          "traefik.http.routers.ziti-console.rule=Host(`zac.${config.cluster.domain}`)"
          "traefik.http.routers.ziti-console.entrypoints=https"
          "traefik.http.routers.ziti-console.middlewares=oauth-auth-redirect@file"
          "traefik.http.routers.ziti-console.tls=true"
          "traefik.http.routers.ziti-console.tls.certresolver=acme"
        ];

        checks = {
          ziti-console-http = {
            interval = "60s";
            timeout = "5s";
            http = "http://127.0.0.1:${toString config.services.ziti-console.portHttp}/login";
          };
        };
      };
    })
    .systemdService;

  # Below, register the remaining ziti controller and edge listener services for consul visibility and health checks,
  # but do not route these services through traefik as doing so creates a potential bandwidth hog on routing machine.
  # We want ZT traefik going directly through zt machine, and need to create a specific Route53 record to zt machine,
  # which can be done in the metal machine declaration with: `route53.domains = ["zt.${cluster.domain}"];`

  systemd.services.ziti-controller-rest-service =
    mkIf config.services.ziti-controller.enable
    (pkgs.consulRegister {
      pkiFiles.caCertFile = "/etc/ssl/certs/ca.pem";
      systemdServiceParent = "ziti-controller";
      service = {
        name = "ziti-controller-rest";
        port = config.services.ziti-controller.portRestApi;
        tags = [
          "ziti"
          "ziti-controller-rest"
        ];

        checks = {
          ziti-controller-rest-tcp = {
            interval = "60s";
            timeout = "5s";
            tcp = "127.0.0.1:${toString config.services.ziti-controller.portRestApi}";
          };
        };
      };
    })
    .systemdService;

  systemd.services.ziti-controller-mgmt-service =
    mkIf config.services.ziti-controller.enable
    (pkgs.consulRegister {
      pkiFiles.caCertFile = "/etc/ssl/certs/ca.pem";
      systemdServiceParent = "ziti-controller";
      service = {
        name = "ziti-controller-mgmt";
        port = config.services.ziti-controller.portManagementApi;
        tags = [
          "ziti"
          "ziti-controller-mgmt"
          "zt.${config.cluster.domain}"
        ];

        checks = {
          ziti-controller-mgmt-tcp = {
            interval = "60s";
            timeout = "5s";
            tcp = "127.0.0.1:${toString config.services.ziti-controller.portManagementApi}";
          };
        };
      };
    })
    .systemdService;

  systemd.services.ziti-router-edge-service =
    mkIf config.services.ziti-router.enable
    (pkgs.consulRegister {
      pkiFiles.caCertFile = "/etc/ssl/certs/ca.pem";
      systemdServiceParent = "ziti-router";
      service = {
        name = "ziti-router-edge";
        port = config.services.ziti-router.portEdgeConnection;
        tags = [
          "ziti"
          "ziti-router-edge"
          "zt.${config.cluster.domain}"
        ];

        checks = {
          ziti-router-edge-tcp = {
            interval = "60s";
            timeout = "5s";
            tcp = "127.0.0.1:${toString config.services.ziti-router.portEdgeConnection}";
          };
        };
      };
    })
    .systemdService;

  systemd.services.ziti-router-fabric-service =
    mkIf config.services.ziti-router.enable
    (pkgs.consulRegister {
      pkiFiles.caCertFile = "/etc/ssl/certs/ca.pem";
      systemdServiceParent = "ziti-router";
      service = {
        name = "ziti-router-fabric";
        port = config.services.ziti-router.portEdgeConnection;
        tags = [
          "ziti"
          "ziti-router-fabric"
          "zt.${config.cluster.domain}"
        ];

        checks = {
          ziti-router-fabric-tcp = {
            interval = "60s";
            timeout = "5s";
            tcp = "127.0.0.1:${toString config.services.ziti-router.portFabricLinks}";
          };
        };
      };
    })
    .systemdService;
}
