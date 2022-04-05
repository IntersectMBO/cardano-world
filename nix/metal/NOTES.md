# Notes

_achtung_ New cluster in `clusters/` remains invisible until staged!

### Generate stuff for `encrypted/`

Must manually create IAM user in the new account with a aws_access_key_id/aws_secret_access_key pair (i.e., not assume role).
Must manually create KMS key, it shuold be symmetrical with defaults.
Put ARN in `clusters/cluster/name/default.nix`

To generate the secrets, run:

```
nix run .#nixosConfigurations.cardano-testnet-core-1.config.secrets.generateScript
```

when generating secrets, the lock file (`.secrets-generate.lock`) doesn't get cleared.

### S3 bucket should be created manually

- Deploying core took more than 90 minutes; after which `/etc/ssl/certs/cert-key.pem` was missing.

Had to run vault-bootstrap systemd service manually (as in the script itself, as root.)

### Terraform clients

Getting these errors consistently.

```

Error: error modifying VPC Peering Connection (pcx-0c1b045682f934dc9) Options: OperationNotPermitted: Peering pcx-0c1b045682f934dc9 is not active. Peering options can be added only to active peerings.
	status code: 400, request id: 398015eb-7d0d-471b-9957-5c12124a2356



Error: error modifying VPC Peering Connection (pcx-097416eafc9287bae) Options: OperationNotPermitted: Peering pcx-097416eafc9287bae is not active. Peering options can be added only to active peerings.
	status code: 400, request id: 5688aa25-1730-4c24-a990-12f24545569a



Error: error modifying VPC Peering Connection (pcx-0dcf3357cdd6ccf0d) Options: OperationNotPermitted: Peering pcx-0dcf3357cdd6ccf0d is not active. Peering options can be added only to active peerings.
	status code: 400, request id: adfd3463-f7b7-4bc3-97c6-f43f5978badf



Error: error modifying VPC Peering Connection (pcx-0a4168be2aa16ec07) Options: OperationNotPermitted: Peering pcx-0a4168be2aa16ec07 is not active. Peering options can be added only to active peerings.
	status code: 400, request id: 77119145-a5c1-4141-aca4-56321c5ac600



Error: error modifying VPC Peering Connection (pcx-08c15669dd466ca88) Options: OperationNotPermitted: Peering pcx-08c15669dd466ca88 is not active. Peering options can be added only to active peerings.
	status code: 400, request id: 119e221e-2566-4349-ac60-1f5dc1f5ca3c



Error: error modifying VPC Peering Connection (pcx-0e1d6904642acc3ae) Options: OperationNotPermitted: Peering pcx-0e1d6904642acc3ae is not active. Peering options can be added only to active peerings.
	status code: 400, request id: ac84f7f4-e091-4bed-b6cf-87c3f87535df

```

Had to untaint as follows:

```
cat <<EOF | xargs -n1 nix run .#clusters.cardano-testnet.tf.clients.terraform -- untaint
aws_vpc_peering_connection_options.eu-central-1
aws_vpc_peering_connection_options.eu-central-1-connect-eu-west-1
aws_vpc_peering_connection_options.eu-central-1-connect-us-east-2
aws_vpc_peering_connection_options.eu-west-1
aws_vpc_peering_connection_options.eu-west-1-connect-us-east-2
aws_vpc_peering_connection_options.us-east-2
EOF
```

then re-run

```
nix run .#clusters.cardano-testnet.tf.clients.plan
nix run .#clusters.cardano-testnet.tf.clients.apply
```

(ACHTUNG: Do not forget the `plan` part first, because rerunning apply on the same plan would just re-create all resources, leaving the existing ones orphaned and sending you on a manual deletion hunt in the AWS web console. Not fun at all!!!)

### Note regarding CA transition

In the transition from one CA to another, the nomad authentication endpoint must be updated manually with the new certificate.
