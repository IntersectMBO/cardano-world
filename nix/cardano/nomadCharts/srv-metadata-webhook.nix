{
  namespace,
}: {
  address_mode = "auto";
  check = [
    {
      name = "live";
      address_mode = "host";
      port = "webhook";
      type = "tcp";
      interval = "10s";
      timeout = "5s";
    }
  ];
  name = "${namespace}-metadata-webhook";
  port = "webhook";
  tags = [
    "\${NOMAD_ALLOC_ID}"
    "ingress"
    "traefik.enable=true"

    # Metadata webhook
    "traefik.http.routers.${namespace}-metadata-webhook.rule=Host(`metadata.world.dev.cardano.org`) && PathPrefix(`/webhook`) && Method(`POST`)"
    "traefik.http.routers.${namespace}-metadata-webhook.entrypoints=https"
    "traefik.http.routers.${namespace}-metadata-webhook.tls=true"
    "traefik.http.routers.${namespace}-metadata-webhook.tls.certresolver=acme"
    "traefik.http.routers.${namespace}-metadata-webhook.middlewares=${namespace}-metadata-cors-headers,${namespace}-metadata-webhook-ratelimit"

    # Middleware (${namespace}-metadata-cors-headers is already declared in srv-metadata-varnish)
    "traefik.http.middlewares.${namespace}-metadata-webhook-ratelimit.ratelimit.average=1"
    "traefik.http.middlewares.${namespace}-metadata-webhook-ratelimit.ratelimit.burst=5"
    "traefik.http.middlewares.${namespace}-metadata-webhook-ratelimit.ratelimit.period=1"
  ];
}
