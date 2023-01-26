{
  imports = [
    ({
      boot.kernelModules = [ "dm_multipath" "dm_round_robin" "ipmi_watchdog" ];
      services.openssh.enable = true;
      system.stateVersion = "22.11";
    }
    )
    ({
      nixpkgs.config.allowUnfree = true;

      boot.initrd.availableKernelModules = [
        "ahci"
        "mpt3sas"
        "nvme"
        "sd_mod"
        "xhci_pci"
      ];
      boot.initrd.kernelModules = [ ];
      boot.kernelModules = [ "kvm-amd" ];
      boot.kernelParams = [ "console=ttyS1,115200n8" ];
      boot.extraModulePackages = [ ];

      hardware.enableAllFirmware = true;
    }
    )
    ({ lib, ... }:
      {
        boot.loader.grub.extraConfig = ''
          serial --unit=0 --speed=115200 --word=8 --parity=no --stop=1
          terminal_output serial console
          terminal_input serial console
        '';
        nix.settings.max-jobs = lib.mkDefault 64;
      }
    )
    ({
      swapDevices = [

        {
          device = "/dev/disk/by-id/ata-MTFDDAV240TDU_221937EC1F94-part2";
        }

      ];

      fileSystems = {

        "/boot" = {
          device = "/dev/disk/by-id/ata-MTFDDAV240TDU_221937EC1F94-part1";
          fsType = "vfat";

        };


        "/scratch" = {
          device = "/dev/disk/by-id/ata-MTFDDAV240TDU_221937EC4DF9-part1";
          fsType = "ext4";

        };


        "/" = {
          device = "zpool/root";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/nix" = {
          device = "zpool/nix";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/var" = {
          device = "zpool/var";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/cache" = {
          device = "zpool/cache";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/var/lib/nomad" = {
          device = "zpool/nomad";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/var/lib/containers" = {
          device = "zpool/containers";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/var/lib/docker" = {
          device = "zpool/docker";
          fsType = "zfs";
          options = [ "defaults" ];
        };


        "/home" = {
          device = "zpool/home";
          fsType = "zfs";
          options = [ "defaults" ];
        };

      };

      boot.loader.efi.efiSysMountPoint = "/boot";
    })
    ({ networking.hostId = "9f29ac5c"; }
    )
    ({ modulesPath, ... }: {
      networking.hostName = "explorer-1";
      networking.useNetworkd = true;


      systemd.network.networks."40-bond0" = {
        matchConfig.Name = "bond0";
        linkConfig = {
          RequiredForOnline = "carrier";
          MACAddress = "b4:96:91:ec:c7:64";
        };
        networkConfig.LinkLocalAddressing = "no";
        dns = [
          "147.75.207.207"
          "147.75.207.208"
        ];
      };


      boot.extraModprobeConfig = "options bonding max_bonds=0";
      systemd.network.netdevs = {
        "10-bond0" = {
          netdevConfig = {
            Kind = "bond";
            Name = "bond0";
          };
          bondConfig = {
            Mode = "802.3ad";
            LACPTransmitRate = "fast";
            TransmitHashPolicy = "layer3+4";
            DownDelaySec = 0.2;
            UpDelaySec = 0.2;
            MIIMonitorSec = 0.1;
          };
        };
      };


      systemd.network.networks."30-enp65s0f0" = {
        matchConfig = {
          Name = "enp65s0f0";
          PermanentMACAddress = "b4:96:91:ec:c7:64";
        };
        networkConfig.Bond = "bond0";
      };


      systemd.network.networks."30-enp65s0f1" = {
        matchConfig = {
          Name = "enp65s0f1";
          PermanentMACAddress = "b4:96:91:ec:c7:65";
        };
        networkConfig.Bond = "bond0";
      };



      systemd.network.networks."40-bond0".addresses = [
        {
          addressConfig.Address = "147.75.84.59/31";
        }
        {
          addressConfig.Address = "2604:1380:4602:2500::1/127";
        }
        {
          addressConfig.Address = "10.12.171.129/31";
        }
      ];
      systemd.network.networks."40-bond0".routes = [
        {
          routeConfig.Gateway = "147.75.84.58";
        }
        {
          routeConfig.Gateway = "2604:1380:4602:2500::";
        }
        {
          routeConfig.Gateway = "10.12.171.128";
        }
      ];
    }
    )
  ];
}
