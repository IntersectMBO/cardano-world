name: environmentName: {
  self,
  pkgs,
  lib,
  config,
  etcEncrypted,
  ...
}: let
  inherit (config.cluster) domain;

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
            allowedIPs = ["192.168.254.254/32"];
            endpoint = "${config.cluster.awsExtNodes.explorer.privateIP}:51820";
            persistentKeepalive = 30;
          }
          # cardano-world monitoring server
          # wg pubkey < <(sops -d ../encrypted/wg/monitoring-private)
          {
            publicKey = "nIxaHgQhzVfh1U/ZgwHdhcDczNJEbHEXWXVAo3EyOWE=";
            allowedIPs = ["192.168.254.100/32"];
            endpoint = "monitoring-wg.${domain}:51820";
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
