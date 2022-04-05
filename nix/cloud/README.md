# Cardano Ops

## Cardano Ops Web UI Resources

- The following resources are available in the cardano ecosystem, all behind oauth2 proxy authentication.
  - [Monitoring webpage](https://monitoring.dev.cardano.org)
    - Used to review and query metrics and logs for cardano processes and nodes.
  - [Nomad webpage](https://nomad.dev.cardano.org)
    - Used to review Nomad job status and related information.
    - Requires ACL token [authentication](https://nomad.dev.cardano.org/ui/settings/tokens) by providing the Nomad token generated in the steps above.
    - Provides UI capability to control Nomad job lifecycle (`Stop`, `Restart`) and interactive inspection (`Exec`) for specific allocations and/or jobs.
  - [Consul webpage](https://consul.dev.cardano.org)
    - Used to review cardano cluster and service status.
  - [Vault webpage](https://vault.dev.cardano.org)
    - Used to review key-value paths utilized in ops configuration.
    - Requires a vault token for sign-in by providing the Vault token generated in the steps above.

## Cardano Ops Namespaces

- The cardano deployed infrastructure is separated into namespaces where jobs can either run in the main testnet namespace, `cardano-testnet-prod` or a different namespace, such as `cardano-testnet-prod`.
- In general, use of the appropriate namespace as a parameter will be required when interacting with the cardano projects.
- Examples of this are:
  - From the Monitoring webpage, some dashboards may include a Nomad namespace parameter near the top of the dashboard; also some queries may be parameterized with namespace
  - From the Nomad webpage, a namespace drop-down selector will be visible in the top left of the UI on many pages
  - From the Nomad CLI, a `-namespace $NAMESPACE` parameter is often required to return appropriate results
  - From the Consul webpage, Consul CLI and consul-template results, registered services will generally have the Nomad namespace embedded in the service name
  - From the Vault webpage and Vault CLI, in the kv/nomad-cluster path, subpaths will be prefixed by Nomad namespace

## Metrics and Logs

- Metrics and logs can be found from the Grafana web UI at: https://monitoring.dev.cardano.org
- Querying logs by job identifier can be done through the "Explore" icon in the left vertical panel.
- If you don't see this "Explore" icon which looks like a compass, request "Editor" access from DevOps.
- Examples of log queries to the `Loki` log datasource in Grafana are:

  ```bash
  # In the "Log labels" field enter the following to search for all logs related to the `cardano-testnet-prod` namespace:
  {namespace="cardano-testnet-prod"}

  # In the "Log labels" field enter the following to search for all logs related to the `cardano-testnet-prod` namespace
  # and filter for example, XYZ events:
  {namespace="cardano-testnet-prod"} |~ "XYZ"
  ```

- A full reference of LogQL is [available](https://grafana.com/docs/loki/latest/logql) as well.

- Logs can also be obtained from the command line with commands such as:

  ```bash
  # Generalized example:
  nomad status -namespace $NS $JOB | grep "$TG.*running" | head -1 | awk '{ print $1 }' | xargs -I{} nomad logs -namespace "$NS" [-stderr] [-tail] [-f] [-n LINES] [-verbose] {} "$TG"

  # Tail and follow a job "devbox" taskgroup in namespace "cardano-testnet-prod":
  TG="devbox"; NS="cardano-testnet-prod"; JOB="miner"; nomad status -namespace "$NS" "$JOB" \
    | grep "$TG.*running" | head -1 | awk '{ print $1 }' \
    | xargs -I{} nomad logs -namespace "$NS" -tail -f {} "$TG"

  # Output from commands above can be redirected to a local file by appending `> $LOG_OUTPUT_FILENAME` for further grep inspection
  ```

## Updating the Cardano source used for Deployments

- The revision of Cardano that Nomad jobs are deployed from are defined in the [`deploy.cue`](deploy.cue) file.
- Different namespaces, or jobs within namespaces can be pinned to their own revisions as needed.

## Updating Package Sources

- TODO

## Deployment: Running a Cardano Job

- Presently, there is no requirement to commit changes from a Cardano job definition to the repository in order to deploy the job.
- To minimize confusion in the team about what job definition is running on the testnet, any changes to Cardano jobs made and deployed should be committed, including updates to docker-image.cue for docker based jobs.

- If the Cardano job to be run is a docker based job, the latest docker image for the job has to be built and pushed to the docker cluster repository before submitting the job utilizing the docker image to Nomad is submitted:

  ```bash
  $ nix run .#dockerImages.$DOCKER_ATTR_NAME.push
  $ nix build .#dockerImagesCue --json | jq -r '.[0].outputs.out' | xargs cat > docker-image.cue
  ```

- To run a cardano job by deploying it to the testnet, execute the following command:
  ```bash
  # Deployments are handled with the `iogo` tool from the `bitte` package
  $ iogo plan $namespace $job
  ```
- To help avoid unwanted changes, a confirmation dialogue will list a diff of the planned changes before deployment actually runs.

- Versioning information about the deployment, including changes from the last version deployed, can be viewed in the Nomad UI in the "Versions" section for a given namespace, e.g. the [cardano-testnet-prod versions](https://nomad.dev.cardano.org/ui/jobs/devbox/versions?namespace=cardano-testnet-prod).

## Admin Ops Section

### Authentication and Authorization

- Authentication is via github tokens.
- Authorization is granted based on the user and/or groups the github tokens belong to.

- Github personal access token (PAT) requirements are:

  - Scope: `read:org` ONLY (under the `admin:org` area).

- For admin credentials, admin role and dev role privileges are assigned through:

  - The `clusters/cardano/testnet/default.nix` file `adminNames` list attribute.
  - The `clusters/cardano/testnet/default.nix` file `developerGithubNames` and `developerGithubTeamNames` list attributes.

- The github team name for dev role is currently `cardano-devs`.

- Since cardano utilizes private repos for packages (including the `cardano` repo itself), a github PAT token should also be set up in a `netrc` file.

  - Typically `netrc` would be set up at `/etc/nix/netrc` for nixos and some nix only installations, or alternatively ~/.config/nix/netrc for nix only installations.
  - For cardano infrastructure, this token is implemented through the `clusters/cardano/testnet/secrets.nix` module using the `encrypted/netrc` file.
  - The `netrc` file will be required for cardano infrastructure machines, and local admin ops machines for repo cloning and building packages.
  - The `bitte-cli` tool also expects a copy of the `netrc` file at `~/.netrc` for an admin ops user to perform terraform related operations.
  - For infrastructure machines a PAT is generated and utilized from a devops cardano github account.
  - For admin ops machines, a PAT is generated and utilized from the local users github account
  - The format for the netrc file is:
    ```bash
    machine github.com login $GITHUB_USER_NAME password $TOKEN
    machine api.github.com login $GITHUB_USER_NAME password $TOKEN
    ```
  - TODO: describe required PAT perms for different `netrc` roles (infra, bitte stack auth, local flake clones).

- Token acquisition commands for bitte stack CLI and UI use:

  ```bash
  # Vault token commands
  vault login -method github -path github-employees
  vault token lookup
  vault print token

  # Nomad and Consul token commands for a desired role (admin or developer)
  vault read -field secret_id nomad/creds/$ROLE
  vault read -field token consul/creds/$ROLE

  # For easy CLI usage, export the tokens to the standard Hashicorp env vars for the desired role
  export NOMAD_TOKEN="$(vault read -field secret_id nomad/creds/$ROLE)"
  export CONSUL_HTTP_TOKEN="$(vault read -field token consul/creds/$ROLE)"
  ```

- Use of `direnv` will automatically export Consul and Nomad developer tokens.
- For admins, `direnv` will automatically export an admin Nomad token.
- If direnv is not available, a vault and Nomad token can be obtained with `iogo login` as an alternative to the token acquisition commands above.

- Alternatively, AWS IAM can be used to obtain an admin vault role rather than github token if a cardano aws admin credentials profile has been configured:
  ```bash
  # Vault admin role via AWS IAM
  vault login -method aws
  ```

### Cluster UI Resources

- The following are bitte standard cluster wide resources for ops use:
  ```bash
  https://monitoring.dev.cardano.org
  https://vault.dev.cardano.org
  https://nomad.dev.cardano.org
  https://consul.dev.cardano.org
  https://traefik.dev.cardano.org
  ```
- Oauth authorization is required for all the above UIs which is obtained by first visiting the monitoring UI, then any UI of choice subsequently.
- These UI resources cover all testnet namespaces (unstable, rXYZ, etc).
- From the monitoring dashboard, pagerDuty integration is enabled.
  - See the monitoring alerts and cardano related pagerduty service configurations.
- From uptimeRobot, publicly visible endpoints are monitored with pagerDuty integration.
  - See [uptimeRobot](https://uptimerobot.com/) and pagerduty service configurations.

### Testnet Namespacing

- In Nomad, testnets are generally maintained in the form of `cardano-testnet-prod-{[un]stable}` for ease of job identification.
- In cardano repo, declarative code to set these namespaces up generally resides in the following locations and may need to be modified periodically as new namespaces are added or retired:

  - Testnet cluster definition [file](https://github.com/input-output-hk/cardano/blob/master/clusters/cardano/testnet/default.nix).
  - cardano extra roles [file](https://github.com/input-output-hk/cardano/blob/master/clusters/cardano/testnet/cardano-extra-roles.nix).
  - cardano host volumes [file](https://github.com/input-output-hk/cardano/blob/master/clusters/cardano/testnet/host-volumes.nix).
  - cardano IAM policies [file](https://github.com/input-output-hk/cardano/blob/master/clusters/cardano/testnet/iam.nix) (for s3 resource permissions).
    - `bitte tf $WORKSPACE plan` followed by `bitte tf $WORKSPACE apply` will need to be run after changes to this file, where `$WORKSPACE` depends on the nix update.
    - Example, for adding a new namespace for s3 backup access, `bitte tf core plan` followed by `bitte tf core apply` would need to be run.
  - CUE deployment [file](https://github.com/input-output-hk/cardano/blob/master/deploy.cue).
  - Unstable namespace associated `*-unstable.cue` job and task files in the `jobs/` directories.

- Presently, enough infra exists for 1 unstable namespace environment to be running (for devOps use), and one release namespace to be running for internal IOHK testnet testing.

  - The client infra is also namespace separated where the client auto-scaler group has an auto-scaler group suffix (asgSuffix) of `unstable` to host the unstable job namespace and the release auto-scalers do not.

- Of the multiple Nomad testnet namespaces which may be running, the `deploy.cue` file will define only 1 as `primary` and all others as `secondary`.
- Generally, the release namespace (rXYZ) is `primary` and unstable is `secondary`.

### Terraform Certificate Quirks

- Currently the ACME certs are generated via terraform and have a 3 month lifespan.
- A warning should be received at devOps email when the certs are close to expiring.
- Terraform will need to be re-run applied, and traefik service on the routing machine restarted for updated certs to be used -- this needs to happen every 3 months until further improvements are made:

  ```bash
  bitte tf core plan
  bitte tf core apply

  bitte ssh routing
  systemctl restart traefik
  exit

  # Repeat as needed:
  bitte info
  bitte ssh $MACHINE_DEPENDING_ON_CERTS
  systemctl restart $SERVICE_DEPENDING_ON_CERTS
  exit
  ```

### Consul Postgres High Availability State

- Patroni utilizes Consul to track Postgres high availability cluster state.
- Patroni Consul key-value configuration store can be found in the consul `Key/Value` UI under `service/$NAMESPACE`.
- Care must be taken to not corrupt or accidentally delete this Consul KV Postgres state; doing so will risk destroying the HA cluster and may require disaster recovery.

### Postgres High Availability (HA) Cluster Bootstrapping: Preparation

- Requirements for bootstrapping a high availability postgres cluster when it doesn't already exist include:
- Verify the namespace the database HA cluster will exist in is already defined in all required places (see the "Testnet Namespacing" section) and recognized by Nomad.
- Verify there are no running Nomad jobs for this namespace and database cluster and all such previous jobs have been properly stopped and purged.
- Verify there is no pre-existing persistent data in the host volumes mount on Nomad clients at `/var/lib/nomad-volumes/$NAMESPACE/postgres-cardano/*`.
- Verify there is no corresponding Consul KV `service/$NAMESPACE-database` key that exists; if there is, delete it.
  - CAREFUL: deleting the wrong KV will unintentionally destroy a different HA cluster or your Vault state!

### Postgres HA: Bootstrap to Clean Working Postgres HA State

- Verify the Vault KV store for the namespace and database component has the `patroniBootstrapMethod` parameter set to `initdb`.
- Start the first patroni dbHA job (`dbHA1`):

  ```bash
  # Example for namespace `cardano-testnet-prod-prod` and postgres high availability node 1
  info iogo plan cardano-testnet-prod-prod dbHA1
  ```

- Allow the above launched Nomad `dbHA1` job to start up and successfully initialize and then run the postgres database.
- Verify with `patronictl -c /secrets/patroni.yml list` from a shell of this namespace and job that the node is showing as `Leader` and the state is `Running`.

- Continue to launch the second patroni dbHA job (`dbHA2`) using similar commands to those shown above.

  - Verify the same items as for the first HA job above.
  - Verify with `patronictl -c /secrets/patroni.yml list` from a shell of this namespace and job that two nodes are now `Running` and one of them shows as `Leader`.

- Start the `backup` job to ensure wal-g saves periodic backups to storage, which together with WAL archives can be used for timeline or point-in-time-recovery.
- Only 1 backup job is required and it will connect to the postgres HA cluster leader to take periodic wal-g backups.
  ```bash
  # Example for namespace `cardano-testnet-prod-prod` periodic wal-g backups
  info iogo plan cardano-testnet-prod-prod backup
  ```

### Patroni Failover and Replica Behavior

- Once a high availability Postgres cluster is running under Patroni:

  - If the leader fails, automatic failover will happen within a short period of time and a replica will become promoted to leader.
  - If a replica fails, Patroni will recognize that and reflect the loss of a node in `patronictl -c /secrets/patroni.yml list`.
  - If a new high availability node is added to the cluster, Patroni will recognize that and reflect the addition of a node in `patronictl -c /secrets/patroni.yml list`.

- The above points mean that generally when a high availability cluster is running, one HA node at a time can be destroyed and re-deployed without harming the overall HA cluster.

  - NOTE: Purging an HA Postgres job will not clear the persistent DB state from the Nomad client it is assigned to.
  - This is generally not a problem as Patroni will automatically utilze some combination of `basebackup`, `wal-g` WAL archives and `pg-rewind` to bring a new replica into sync with the cluster.
  - If a new replica is having trouble joining a cluster, try clearing the persistent DB state from _only_ that HA node's persistent volume before reallocating the job.

- Patroni allows triggering a manual failover.

### Postgres Recovery Options: Restoring Data to A New Cluster

- Patroni data restoration from wal-g backups and WAL archives requires patroni to be cleared of old cluster state and the restoration to happen during cluster re-initialization
- Be aware that wal-g backups and WAL archives are BOTH required for either a timeline or point in time restore.
- The wal-g backup alone, without some minimal number of WAL archives, is not self-consistent for a single file database restore.

### Postgres Bootstrap to Timeline Recovery with Wal-g

- Repeat the same procedure as in the `Postgres HA: Bootstrap to Clean Working Postgres HA State` section, with the following modifications or verifications:
  - Verify the Vault KV store for the namespace and database component has the `patroniBootstrapMethod` parameter set to `walg_timeline`.
  - Verify the Vault KV store for the namespace and database component has the `patroniBootstrapMethodWalgTimeline` parameter set to the timeline of your choice to which the cluster should be restored.
- NOTE:
  - See postgres documentation for details on timelines.
  - The `backup` job will only retain `walgDaysToRetain` days worth of wal-g backups and WAL files (timeline history files are retained indefinitely), with a minimum parameter value of 2 days.

### Postgres Bootstrap to Point-In-Time-Recovery (PITR) with Wal-g

- Repeat the same procedure as in the `Postgres HA: Bootstrap to Clean Working Postgres HA State` section, with the following modifications or verifications:
  - Verify the Vault KV store for the namespace and database component has the `patroniBootstrapMethod` parameter set to `walg_pitr`.
  - Verify the Vault KV store for the namespace and database component has the `patroniBootstrapMethodWalgPitrTimeline` parameter set to the timeline of your choice to which the cluster should be restored.
  - Verify the Vault KV store for the namespace and database component has the `patroniBootstrapMethodWalgPitrTimestamp` parameter set to the timestamp recovery date to which the cluster should be restored.
- NOTE:
  - See postgres documentation for details on timelines.
  - The timeline specified should be consistent with the timestamp provided, otherwise, point-in-time-recovery will likely fail.
  - The `backup` job will only retain `walgDaysToRetain` days worth of wal-g backups and WAL files (timeline history files are retained indefinitely), with a minimum parameter value of 2 days.

### Additional Postgres HA Disaster Recovery Notes

- The above sections describe procedures to restore a high availability database cluster from backup files to specific timeline or specific point-in-time by restoring to a fresh cluster.
- To restore a database to the cluster without first destroying and then re-initializing the whole cluster, the following approach can be taken:
  - Enter a shell with the patroni postgres leader.
  - Enter a psql shell.
  - Prevent connections to the database that is to be restored from backup (ex: pg_dump) and then terminate all connections.
  - Drop the database that is to be restored from backup.
  - Re-create the database this is to be restored from backup, and grant database role permissions as needed for the application.
  - Exit the psql shell and restore the database to the patroni leader from backup (ex: pg_restore).
  - Migrate and seed the restored database as needed, typically by restarting a backend component which will re-run migrations and seeding.
- See example commands in the `Useful Commands` section below.

### Useful Commands

- The following are useful Consul commands:

  ```bash
  # Check on the details of a Consul token
  curl -H "X-Consul-Token: $CONSUL_HTTP_TOKEN" $CONSUL_HTTP_ADDR/v1/acl/token/self | jq .

  # Check consul members
  consul members

  # Force ejection of a failed consul member
  consul force-leave -prune $MEMBER

  # Check on the Consul raft peer status
  consul operator raft list-peers

  # Deregister a ghost node which no longer exists but has left stale service checks behind
  consul catalog nodes
  curl -v -XPUT \
    -H 'Content-Type: application/json' \
    -H "X-Consul-Token: $CONSUL_HTTP_TOKEN" \
    -d "{\"Datacenter\":\"$DATACENTER\",\"Node\":\"$NODE\"}" \
    $CONSUL_HTTP_ADDR/v1/catalog/deregister

  # Deregister a ghost service (and associated checks) which no longer exists
  curl -v -XPUT \
    -H 'Content-Type: application/json' \
    -H "X-Consul-Token: $CONSUL_HTTP_TOKEN" \
    -d "{\"Datacenter\":\"$DATACENTER\",\"Node\":\"$NODE\",\"ServiceID\":\"$SERVICEID\"}" \
    $CONSUL_HTTP_ADDR/v1/catalog/deregister

  # For the deregistration API, parameters are in the format of:
  $DATACENTER -> Obtained from `consul catalog datacenters`
  $NODE       -> Obtained from first column of `consul catalog nodes`
  $SERVICEID  -> Obtained from Consul UI, long service name under services instance description
              -> Of the form: `_nomad_task-...`
              -> Not easily obtainable by CLI

  # For obtaining information on a token:
  curl -s "$CONSUL_HTTP_ADDR/v1/acl/token/self?token=$CHECK_TOKEN" | jq .

  # For obtaining information on a token accessor id:
  consul acl token read -id $ID

  # For obtaining service information on services hidden in Consul UI (ex: sidecar proxies):
  consul catalog services
  curl -s -H "x-consul-token: $CONSUL_HTTP_TOKEN" "https://consul.dev.cardano.org/v1/health/service/$SERVICE?dc=$DC_REGION" | jq .
  ```

- The following are useful Nomad commands:

  ```bash
  # Useful for obtaining a bash shell from a job and task without looking up an allocation id manually:
  # Example:
  NS=cardano-testnet-prod-prod; JOB=databaseHA1; TASK=postgres-patroni; nomad exec -task $TASK -namespace $NS $(nomad status -namespace $NS $JOB | grep -E '.*run\s+running' | cut -f1 -d' ') /bin/bash

  # Purge a job in a particular namespace
  # WARNING -- THERE IS NO CONFIRM!
  nomad job stop -purge -namespace $NAMESPACE $JOB

  # Deploy a job to a given namespace
  RUST_LOG=info iogo plan $NAMESPACE frontend

  # List nomad clients
  nomad node status -verbose

  # Manually purge a nomad node which is down rather than waiting for it to timeout
  curl -XPOST -H "X-Nomad-Token: $NOMAD_TOKEN" $NOMAD_ADDR/v1/node/$NODE/purge
  ```

- The following are useful Vault commands:

  ```
  # List vault versions in the cluster when backended by Consul:
  bitte ssh -p -- 'printf "%-50s %s\n" "$(hostname):" "$(pgrep -af "[v]ault agent -config" | grep -oP "\Kvault-bin-1.[0-9].[0-9]")"'

  # List vault versions in the cluster when backended by Raft:
  vault operator raft list-peers
  ```

- The following are useful postgres related commands:

  ```
  # To disable and terminate connections to a database so it can be dropped
  UPDATE pg_database SET datallowconn = false WHERE datname = '$DB';
  SELECT pg_terminate_backend(pg_stat_activity.pid) FROM pg_stat_activity WHERE pg_stat_activity.datname = '$DB' AND pid <> pg_backend_pid();

  # To dump and restore a database
  pg_dump -h $HOST -U $USER -d $DB_TO_BACKUP -F c -Z 9 > $BACKUP_NAME.cpgdump

  # $DB_TO_RESTORE must already exist!
  pg_restore -v -h $HOST -U $USER -d $DB_TO_RESTORE $BACKUP_NAME.cpgdump

  # Show all high-availability postgres nodes and their current status
  patronictl -c /secrets/patroni.yml list

  # Switch leader to another node; must be executed from current leader postgres allocation
  patronictl -c /secrets/patroni.yml switchover

  # See all current wal-g backups
  wal-g backup-list

  # Push a new full wal-g backup manually
  wal-g backup-push $PGDATA             \
    --full                              \
    --pghost $HOST                      \
    --pgport $PORT                      \
    --pguser $USER                      \
    --pgdatabase $INIT_CONN_DB          \
    --walg-[s3|file]-prefix $STORE_PATH

  # Show current wal archived segments, timelines, relationships, status
  wal-g wal-show

  # Verify wal archived integrity
  wal-g wal-verify integrity            \
    --pghost $HOST                      \
    --pgport $PORT                      \
    --pguser $USER                      \
    --pgdatabase $INIT_CONN_DB          \
    --walg-[s3|file]-prefix $STORE_PATH

  # Verify wal archived timeline
  wal-g wal-verify timeline             \
    --pghost $HOST                      \
    --pgport $PORT                      \
    --pguser $USER                      \
    --pgdatabase $INIT_CONN_DB          \
    --walg-[s3|file]-prefix $STORE_PATH

  # Query for a psql shell
  psql -h $HOST -U $USER -d $INIT_CONN_DB

  # Query to directly report the current timeline
  select timeline_id from pg_control_checkpoint();

  # Query to determine if a postgres node is a primary or standby server role:
  # f -> primary
  # t -> standby
  select pg_is_in_recovery();
  ```

- The following are useful s3 related commands:
  ```
  # Count objects in a bucket at a particular path -- example: count the wal archived segments for the unstable environment
  aws s3api list-objects --bucket iohk-cardano-bitte --prefix "cardano-testnet-prod/backups/wal-g/wal_005" --output json --query "[length(Contents[])]"
  ```

### Consul Connect Notes

- Currently the connect sidecar, envoy, is limited to defining 1 service port per sidecar.
- Knowing where to declare ports, not declare ports and how to declare ports in connect jobs is tricky. In bridge mode, here is a summary table:

```
| Stanza       | Type               | Declaration                            | Example                                   | Notes                                                 |
|--------------|--------------------|----------------------------------------|-------------------------------------------|-------------------------------------------------------|
| network.port | connect service    | undeclared                             | N/A                                       | Declare instead in `service.port` as a numeric string |
| network.port | selective exposure | dynamic host label                     | `port: { exposedPath: {} }`               | Use `proxy.expose.path.listener_port`                 |
| network.port | full exposure      | dynamic host with internal ns listener | `port: { envoyPrometheus: { to: 9091 } }` | Use `proxy.config.envoy_prometheus_bind_addr`         |
| service.port | connect service    | declared as numeric string             | `port: "6000"`                            | Refers to the port of the internal ns app listener    |
| service.port | selective exposure | undeclared                             | N/A                                       | Use `proxy.expose.path.listener_port`                 |
| service.port | full exposure      | undeclared                             | N/A                                       | Use `proxy.config.envoy_prometheus_bind_addr`         |
| check.port   | connect service    | blank string                           | `check: "checkName" { port: "" }`         | Requires `check: "checkName" { expose: true }` also   |
```

- The connect sidecar_task stanza can clobber default config and cause unexpected network behavior. TODO: Improve the sidecar_task stanza type schema to include only elements explicitly declared in the job config.
- An envoy admin page with a large amount of diagnostic information is available, but by default is listening on a port at ip `127.0.0.2`. While configuring the admin listener to a specific connect network namespace ip and port is possible, there have been bugs with elevated envoy CPU consumption due to listener assignment. So rather than interfere with automatic admin listener placement, the envoy admin panel can be accessed by http1.1 compatible console browsing, like the following example:

```
bitte ssh $CLIENT_NODE
docker ps
docker exec -tiu 0 $ENVOY_CONTAINER_ID /bin/bash
apt update && apt install -y net-tools elinks
netstat -tupln
elinks $ENVOY_ADMIN_ADDR
```

- Connect is designed largely for cattle, not pets, so connect service application members which need to distinguish themselves from their peers can be tricky. In this case, Consul service resolvers may be required to define service subsets. Consul custom query definitions are also another possibility. An example of this may be distinguishing postgres service components between leader and replica in a high availability cluster.
- When running multiple envoy sidecars in the same taskgroup, the sidecar network namespace is shared and port assignments between sidecars must not overlap or at least one sidecar will enter a CPU consumption loop while trying to continuously bind an already allocated port.
