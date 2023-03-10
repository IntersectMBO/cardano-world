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
        "xhci_pci"
        "ahci"
        "usbhid"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [ ];
      boot.kernelModules = [ "kvm-intel" ];
      boot.kernelParams = [ "console=ttyS1,115200n8" ];
      boot.extraModulePackages = [ ];

      hardware.enableAllFirmware = true;
    }
    )
    ({ lib, ... }:
      {
        boot.loader = {
          systemd-boot.enable = true;
          efi.canTouchEfiVariables = true;
        };
        nix.settings.max-jobs = lib.mkDefault 64;
      }
    )
    ({
      swapDevices = [

        {
          device = "/dev/disk/by-id/ata-Micron_5300_MTFDDAK480TDT_2204345F6F6C-part2";
        }

      ];

      fileSystems = {

        "/boot" = {
          device = "/dev/disk/by-id/ata-Micron_5300_MTFDDAK480TDT_2204345F6F6C-part1";
          fsType = "vfat";

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
    ({ networking.hostId = "dd2d5667"; }
    )
    ({ modulesPath, ... }: {
      networking.hostName = "explorer";
      networking.useNetworkd = true;


      systemd.network.networks."40-bond0" = {
        matchConfig.Name = "bond0";
        linkConfig = {
          RequiredForOnline = "carrier";
          MACAddress = "e8:eb:d3:6d:fe:2a";
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


      systemd.network.networks."30-enp1s0f0np0" = {
        matchConfig = {
          Name = "enp1s0f0np0";
          PermanentMACAddress = "e8:eb:d3:6d:fe:2a";
        };
        networkConfig.Bond = "bond0";
      };


      systemd.network.networks."30-enp1s0f1np1" = {
        matchConfig = {
          Name = "enp1s0f1np1";
          PermanentMACAddress = "e8:eb:d3:6d:fe:2b";
        };
        networkConfig.Bond = "bond0";
      };



      systemd.network.networks."40-bond0".addresses = [
        {
          addressConfig.Address = "147.75.85.167/31";
        }
        {
          addressConfig.Address = "2604:1380:4602:2500::5/127";
        }
        {
          addressConfig.Address = "10.12.171.133/31";
        }
      ];
      systemd.network.networks."40-bond0".routes = [
        {
          routeConfig.Gateway = "147.75.85.166";
        }
        {
          routeConfig.Gateway = "2604:1380:4602:2500::4";
        }
        {
          routeConfig.Gateway = "10.12.171.132";
        }
      ];
    }
    )
  ];
}
