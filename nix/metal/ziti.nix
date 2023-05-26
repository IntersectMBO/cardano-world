{lib, self, pkgs, ...}: let
  awsExt-to-equinix-host-v1 = builtins.toJSON {
    allowedAddresses = ["10.12.171.0/24"];
    allowedPortRanges = [
      {
        low = 0;
        high = 65535;
      }
    ];
    allowedProtocols = ["tcp" "udp"];
    forwardAddress = true;
    forwardPort = true;
    forwardProtocol = true;
    listenOptions = {identity = "$tunneler_id.name";};
  };

  awsExt-to-equinix-intercept-v1 = builtins.toJSON {
    addresses = ["10.12.171.0/24"];
    dialOptions = {
      connectTimeoutSeconds = 15;
      identity = "$dst_ip";
    };
    portRanges = [
      {
        low = 0;
        high = 65535;
      }
    ];
    protocols = ["tcp" "udp"];
    sourceIp = "";
  };

  equinix-to-awsExt-host-v1 = builtins.toJSON {
    allowedAddresses = [
      "172.16.0.0/16"
      "10.24.0.0/16"
      "10.32.0.0/16"
      "10.52.0.0/16"
    ];
    allowedPortRanges = [
      {
        low = 0;
        high = 65535;
      }
    ];
    allowedProtocols = ["tcp" "udp"];
    forwardAddress = true;
    forwardPort = true;
    forwardProtocol = true;
  };

  equinix-to-awsExt-intercept-v1 = builtins.toJSON {
    addresses = [
      "172.16.0.0/16"
      "10.24.0.0/16"
      "10.32.0.0/16"
      "10.52.0.0/16"
    ];
    dialOptions = {
      connectTimeoutSeconds = 15;
      identity = "";
    };
    portRanges = [
      {
        low = 0;
        high = 65535;
      }
    ];
    protocols = ["tcp" "udp"];
    sourceIp = "";
  };
in {
  boot.kernel.sysctl."net.ipv4.conf.all.forwarding" = true;

  services = {
    ziti-router.enable = true;
    ziti-console.enable = true;
    ziti-edge-tunnel = {
      enable = true;
      package = self.inputs.openziti.packages.${pkgs.system}.ziti-edge-tunnel_latest_large_tcp;
    };

    ziti-controller = {
      enable = true;
      extraBootstrapPost = ''
        # ----------------------------------------------

        # Provision expected identities for enrollment to fulfill the service requirements
        # Note manual completion of enrollment on the target devices currently required

        mkdir -p enroll-jwts

        ziti edge create identity device zt-zet.ziti \
          --jwt-output-file enroll-jwts/cardano-world-zt-zet.jwt \
          --role-attributes gw-zet

        ziti edge create identity device explorer-1.ziti \
          --jwt-output-file enroll-jwts/cardano-world-explorer-1.jwt \
          --role-attributes explorer-1.ziti

        ziti edge create identity device explorer-2.ziti \
          --jwt-output-file enroll-jwts/cardano-world-explorer-2.jwt \
          --role-attributes explorer-2.ziti

        ziti edge create identity device explorer-3.ziti \
          --jwt-output-file enroll-jwts/cardano-world-explorer-3.jwt \
          --role-attributes explorer-3.ziti

        # ----------------------------------------------

        # Create awsExt-to-equinix service
        # shellcheck disable=SC2016
        ziti edge create config awsExt-to-equinix-host-v1 host.v1 '${awsExt-to-equinix-host-v1}'

        # shellcheck disable=SC2016
        ziti edge create config awsExt-to-equinix-intercept-v1 intercept.v1 '${awsExt-to-equinix-intercept-v1}'

        ziti edge create service \
          awsExt-to-equinix \
          --configs awsExt-to-equinix-host-v1 \
          --configs awsExt-to-equinix-intercept-v1 \
          --encryption ON \
          --role-attributes awsExt-to-equinix

        ziti edge create service-policy \
          awsExt-to-equinix-dial \
          Dial \
          --identity-roles '#gw-zet' \
          --service-roles '@awsExt-to-equinix' \
          --semantic "AnyOf"

        ziti edge create service-policy \
          awsExt-to-equinix-bind \
          Bind \
          --identity-roles '#equinix' \
          --service-roles '@awsExt-to-equinix' \
          --semantic "AnyOf"

        # ----------------------------------------------

        # Create equinix-to-aws service
        ziti edge create config equinix-to-awsExt-host-v1 host.v1 '${equinix-to-awsExt-host-v1}'
        ziti edge create config equinix-to-awsExt-intercept-v1 intercept.v1 '${equinix-to-awsExt-intercept-v1}'

        ziti edge create service \
          equinix-to-awsExt \
          --configs equinix-to-awsExt-host-v1 \
          --configs equinix-to-awsExt-intercept-v1 \
          --encryption ON \
          --role-attributes equinix-to-awsExt

        ziti edge create service-policy \
          equinix-to-awsExt-dial \
          Dial \
          --identity-roles '#equinix' \
          --service-roles '@equinix-to-awsExt' \
          --semantic "AnyOf"

        ziti edge create service-policy \
          equinix-to-awsExt-bind \
          Bind \
          --identity-roles '#gw-zet' \
          --service-roles '@equinix-to-awsExt' \
          --semantic "AnyOf"
      '';
    };
  };
}
