{
  config,
  self,
  etcEncrypted,
  ...
}: {
  secrets = {
    install = {
      github = {
        inputType = "binary";
        outputType = "binary";
        source = "${etcEncrypted}/netrc";
        target = /etc/nix/netrc;
        script = ''
          chmod 0600 /etc/nix/netrc
        '';
      };
    };
  };
}
