{
  namespace,
}: {
  address_mode = "auto";
  check = [
    {
      name = "ping";
      address_mode = "host";
      port = "varnish";
      path = "/ping";
      type = "http";
      interval = "10s";
      timeout = "5s";
    }
  ];
  name = "${namespace}-metadata-varnish";
  port = "varnish";
  tags = [
    "\${NOMAD_ALLOC_ID}"
    "ingress"
    "traefik.enable=true"

    # Metadata get
    "traefik.http.routers.${namespace}-metadata.rule=Host(`metadata.world.dev.cardano.org`,`metadata.cardano-testnet.iohkdev.io`) && PathPrefix(`/metadata`) && Method(`GET`,`OPTIONS`)"
    "traefik.http.routers.${namespace}-metadata.entrypoints=https"
    "traefik.http.routers.${namespace}-metadata.tls=true"
    "traefik.http.routers.${namespace}-metadata.tls.certresolver=acme"
    "traefik.http.routers.${namespace}-metadata.middlewares=${namespace}-metadata-cors-headers"

    # Metadata batch
    "traefik.http.routers.${namespace}-metadata-batch.rule=Host(`metadata.world.dev.cardano.org`,`metadata.cardano-testnet.iohkdev.io`) && PathPrefix(`/metadata/query`) && Method(`POST`,`OPTIONS`)"
    "traefik.http.routers.${namespace}-metadata-batch.entrypoints=https"
    "traefik.http.routers.${namespace}-metadata-batch.tls=true"
    "traefik.http.routers.${namespace}-metadata-batch.tls.certresolver=acme"
    "traefik.http.routers.${namespace}-metadata.middlewares=${namespace}-metadata-cors-headers,${namespace}-metadata-ratelimit"

    # Middleware
    "traefik.http.middlewares.${namespace}-metadata-cors-headers.headers.addVaryHeader=true"
    "traefik.http.middlewares.${namespace}-metadata-cors-headers.headers.accessControlAllowMethods=GET,OPTIONS,POST"
    "traefik.http.middlewares.${namespace}-metadata-cors-headers.headers.accessControlAllowHeaders=User-Agent,X-Requested-With,Content-Type"

    "traefik.http.middlewares.${namespace}-metadata-ratelimit.ratelimit.average=10"
    "traefik.http.middlewares.${namespace}-metadata-ratelimit.ratelimit.burst=20"
    "traefik.http.middlewares.${namespace}-metadata-ratelimit.ratelimit.period=1"
  ];
}
