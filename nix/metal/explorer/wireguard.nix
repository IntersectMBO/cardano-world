name: environmentName: {
  self,
  pkgs,
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
          # cardano-world explorer gateway
          # wg pubkey < <(sops -d ../encrypted/wg/explorer-private)
          {
            publicKey = "4DEOtdKOu8h284ZwjOsd/cKqSmuQnI+Jy2yiUPxG9B8=";
            allowedIPs = ["192.168.254.253/32"];
            endpoint = "${config.cluster.awsExtNodes.explorer.privateIP}:51820";
            persistentKeepalive = 30;
          }
          # mainnet explorer gateway
          # wg pubkey < <(sops -d ../encrypted/wg/explorer-gateway-private)
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
    extraPackages = [pkgs.wireguard-tools];
    script = ''
      chmod 0400 /etc/wireguard/private.key
      wg pubkey < /etc/wireguard/private.key > /etc/wireguard/public.key
    '';
  };
}
