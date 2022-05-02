#! /usr/env bash

trap 'echo "$(date -u +"%b %d, %y %H:%M:%S +0000"): Caught SIGINT -- exiting" && exit 0' INT

[ -z "${envName:-}" ] && echo "envName env var must be set -- aborting" && exit 1
[ -z "${stateDir:-}" ] && echo "stateDir env var must be set -- aborting" && exit 1

stateDir="${stateDir/#\~/$HOME}"

S3_BASE="s3://iohk-moe-public"

tmpdir=$(mktemp --directory)
pushd "$tmpdir"
tar -zcvf "db-${envName}.tgz" --directory "${stateDir}" .
sha256sum "db-${envName}.tgz" >"db-${envName}.tgz.sha256"
aws s3 cp "db-${envName}.tgz" "$S3_BASE/db-${envName}.tgz"
aws s3 cp "db-${envName}.tgz.sha256" "$S3_BASE/db-${envName}.tgz.sha256"
popd "$tmpdir"
rm -r "$tmpdir"
