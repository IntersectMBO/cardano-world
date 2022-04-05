{
  pkgs,
  lib,
  config,
  self,
  ...
}: {
  services.consul.logLevel = lib.mkForce "trace";
  services.nomad.plugin.docker = {
    auth.config = "/var/lib/nomad/.docker/config.json";
    volumes.enabled = true;
    allow_caps = [
      "audit_write"
      "chown"
      "dac_override"
      "fowner"
      "fsetid"
      "kill"
      "mknod"
      "net_bind_service"
      "setfcap"
      "setgid"
      "setpcap"
      "setuid"
      "sys_chroot"
      "net_raw"
      # Allow ping
      "sys_ptrace"
      # Allow strace
    ];
  };
  secrets.install.docker-login = {
    source = config.secrets.encryptedRoot + "/docker-passwords.json";
    target = /run/keys/docker-passwords-decrypted;
    script = ''
      export PATH="${lib.makeBinPath (with pkgs; [coreutils jq])}"

      mkdir -p /root/.docker

      while [ -f "/root/.docker/config.lock" ]; do
        echo "Sleeping one second for docker config lock..."
        sleep 1
      done

      trap "rm /root/.docker/config.lock" EXIT
      touch /root/.docker/config.lock

      hashed="$(jq -r -e < /run/keys/docker-passwords-decrypted .password)"
      hashed_infra="$(jq -r -e < /run/keys/docker-passwords-decrypted .password_infra)"
      auth="$(echo -n "developer:$hashed" | base64)"
      auth_infra="$(echo -n "developer:$hashed_infra" | base64)"
      ua="Docker-Client/19.03.12 (linux)"

      [ -f "/root/.docker/config.json" ] && CONTENT="$(< /root/.docker/config.json)" || CONTENT="{}"

      echo "$CONTENT" \
        | jq --arg auth "$auth" '.auths."docker.${config.cluster.domain}".auth = $auth' \
        | jq --arg auth "$auth_infra" '.auths."docker.infra.aws.iohkdev.io".auth = $auth' \
        | jq --arg ua "$ua" '.HttpHeaders."User-Agent" = $ua' \
        > /root/.docker/config.json

      mkdir -p /var/lib/nomad/.docker
      cp /root/.docker/config.json /var/lib/nomad/.docker/config.json
    '';
  };
  # Bitte profiles/docker-registry.nix auto-generates docker-passwords.json with a local cluster registry,
  # so better to keep supplemental static creds seperate to avoid overwrites.
  secrets.install.docker-supplemental = {
    source = config.secrets.encryptedRoot + "/docker-supplemental.json";
    target = /run/keys/docker-supplemental-decrypted;
    script = ''
      export PATH="${lib.makeBinPath (with pkgs; [coreutils jq])}"

      mkdir -p /root/.docker

      while [ -f "/root/.docker/config.lock" ]; do
        echo "Sleeping one second for docker config lock..."
        sleep 1
      done

      trap "rm /root/.docker/config.lock" EXIT
      touch /root/.docker/config.lock

      [ -f "/root/.docker/config.json" ] && CONTENT="$(< /root/.docker/config.json)" || CONTENT="{}"

      for registry in $(jq -r -e '.auths | keys | .[]' < /run/keys/docker-supplemental-decrypted); do
        hashedUser="$(jq -r -e < /run/keys/docker-supplemental-decrypted .auths.\"''${registry}\".user)"
        hashedPass="$(jq -r -e < /run/keys/docker-supplemental-decrypted .auths.\"''${registry}\".password)"
        auth="$(echo -n "$hashedUser:$hashedPass" | base64)"
        OUTPUT="$(echo "$CONTENT" | jq --arg auth "$auth" --arg registry "$registry" '.auths[$registry].auth = $auth')"
        CONTENT="$OUTPUT"
      done

      # Define credHelpers
      echo "$CONTENT" \
        | jq '.credHelpers."895947072537.dkr.ecr.us-east-2.amazonaws.com" = "ecr-login"' \
        > /root/.docker/config.json

      mkdir -p /var/lib/nomad/.docker
      cp /root/.docker/config.json /var/lib/nomad/.docker/config.json
    '';
  };
}
