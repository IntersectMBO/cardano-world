name: environmentName: {
  self,
  lib,
  config,
  etcEncrypted,
  ...
}: let
  inherit (config.services.cardano-node) system;
  inherit (self.${system}.cardano.environments.${environmentName}) domain;

  # Obtain the explorer name index number
  explorerNum = builtins.elemAt (lib.splitString "-" name) 1;
in {
  networking = {
    # See the bitte profiles/multicloud/equinix.nix for equinix firewall handling
    firewall.allowedUDPPorts = lib.mkForce [51820];
    wireguard = {
      enable = true;
      interfaces.wg = {
        listenPort = 51820;
        ips = ["192.168.254.${explorerNum}/32"];
        privateKeyFile = "/etc/wireguard/private.key";
        peers = [
          # explorer gateway
          {
            publicKey = "tW/7xjjD2vobYtM4aK/2E3M7EjgfUhBWxISLy9CUVxw=";
            allowedIPs = ["192.168.254.254/32"];
            endpoint = "explorer.${domain}:51820";
            persistentKeepalive = 30;
          }
        ];
      };
    };
  };

  secrets.install.wg-private = {
    source = "${etcEncrypted}/wg/explorer-${explorerNum}-private";
    target = "/etc/wireguard/private.key";
    outputType = "binary";
    script = ''
      chmod 0400 /etc/wireguard/private.key
    '';
  };

  secrets.install.wg-public = {
    source = "${etcEncrypted}/wg/explorer-${explorerNum}-public";
    target = "/etc/wireguard/public.key";
    outputType = "binary";
  };
}
