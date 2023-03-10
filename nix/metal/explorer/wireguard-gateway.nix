name: environmentName: {
  self,
  pkgs,
  lib,
  config,
  etcEncrypted,
  ...
}: let
  inherit (self.x86_64-linux.cardano.environments.${environmentName}.auxConfig) explorerActiveBackends;

  # Obtain the explorer name index number
  explorerNum = backendName: builtins.elemAt (lib.splitString "-" backendName) 1;
in {
  networking = {
    # See the bitte profiles/multicloud/equinix.nix for equinix firewall handling
    firewall.allowedUDPPorts = lib.mkForce [51820];
    wireguard = {
      enable = true;
      interfaces.wg = {
        listenPort = 51820;
        # There should be only 1 traefik gateway per explorer environment, so we can fix this ip
        # Mainnet legacy cluster traefik gateway is reserved as 192.168.254.254 so use the next available.
        ips = ["192.168.254.253/32"];
        privateKeyFile = "/etc/wireguard/private.key";
        peers = map (backend: {
          inherit (backend) publicKey;
          allowedIPs = ["192.168.254.${explorerNum backend.name}/32"];
          endpoint = "${config.cluster.awsExtNodes.${backend.name}.privateIP}:51820";
          persistentKeepalive = 30;
        }) explorerActiveBackends;
      };
    };
  };

  secrets.install.wg-private = {
    source = "${etcEncrypted}/wg/${name}-private";
    target = "/etc/wireguard/private.key";
    outputType = "binary";
    extraPackages = [pkgs.wireguard-tools];
    script = ''
      chmod 0400 /etc/wireguard/private.key
      wg pubkey < /etc/wireguard/private.key > /etc/wireguard/public.key
    '';
  };
}
