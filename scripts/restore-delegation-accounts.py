#!/usr/bin/env nix-shell
#!nix-shell -i python -p python3Packages.docopt python3Packages.cbor2

"""restore-delegation-accounts

Usage:
    restore-delegation-accounts [--testnet-magic INT] [--signing-key-file FILE] [--wallet-mnemonic FILE] [--delegation-index INT]

Options:
    -h --help                    Show this screen
    -t --testnet-magic <INT>     Testnet Magic
    -s --signing-key-file <FILE> Signing Key
    -w --wallet-mnemonic <FILE>  mnemonic file cardano-address uses
    -d --delegation-index <INT>  delegation-index to withdraw rewards from, undelegate, deregister and register for faucet re-use

"""
import json
import os
import subprocess
import tempfile
from docopt import docopt
from pathlib import Path

arguments = docopt(__doc__, version="restore-delegation-accounts 0.0")
network_args = []

if arguments["--signing-key-file"] and os.path.exists(arguments["--signing-key-file"]):
    utxo_signing_key = Path(arguments["--signing-key-file"])

else:
    print("Must specify signing key file")
    exit(1)

if arguments["--delegation-index"]:
    d_idx = int(arguments["--delegation-index"])
else:
    print("Must specify a delegation index")
    exit(1)

if arguments["--testnet-magic"]:
    network_args = ["--testnet-magic", arguments["--testnet-magic"]]
else:
    network_args = ["--mainnet"]

last_txin = ""


def initialize_root_key(mnemonic):
    cli_args = [
        "cardano-address",
        "key",
        "from-recovery-phrase",
        "Shelley",
    ]
    p = subprocess.run(cli_args, input=mnemonic, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error converting mnemonic to root key")
    return p.stdout.rstrip()


def derive_account_key(root_key, public=False):
    return derive_child_key(root_key, "1852H/1815H/0H", public=public)


def derive_payment_address_cli_skey(payment_key_file):
    with tempfile.NamedTemporaryFile("w+") as payment_vkey:
        cli_args = [
            "cardano-cli",
            "key",
            "verification-key",
            "--signing-key-file",
            payment_key_file,
            "--verification-key-file",
            payment_vkey.name,
        ]
        p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error converting mnemonic to root key")
        cli_args = [
            "cardano-cli",
            "address",
            "build",
            "--verification-key-file",
            payment_vkey.name,
            *network_args,
        ]
        p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error converting mnemonic to root key")
        return p.stdout.rstrip()


# TODO: this doesn't work for mainnet
def derive_stake_address(stake):
    cli_args = ["cardano-address", "address", "stake", "--network-tag", "testnet"]
    p = subprocess.run(cli_args, input=stake, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error deriving account key from root key")
    return p.stdout.rstrip()


def derive_payment_address(payment):
    cli_args = ["cardano-address", "address", "payment", "--network-tag", "testnet"]
    p = subprocess.run(cli_args, input=payment, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error deriving account key from root key")
    return p.stdout.rstrip()


def derive_delegation_address(payment_address, stake_vkey):
    cli_args = ["cardano-address", "address", "delegation", stake_vkey]
    p = subprocess.run(cli_args, input=payment_address, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error deriving account key from root key")
    return p.stdout.rstrip()


def derive_child_key(key, derivation, public=False, chain_code=True):
    if chain_code:
        chain_code_arg = "--with-chain-code"
    else:
        chain_code_arg = "--without-chain-code"
    cli_args = ["cardano-address", "key", "child", derivation]
    p = subprocess.run(cli_args, input=key, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error deriving account key from root key")
    skey = p.stdout.rstrip()
    if public:
        cli_args = ["cardano-address", "key", "public", chain_code_arg]
        p = subprocess.run(cli_args, input=skey, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            raise Exception("Unknown error retrieving public key")
        return p.stdout.rstrip()
    else:
        return skey


def generate_stake_skey(stake_xsk, file):
    cli_args = [
        "bash",
        "-c",
        "cardano-cli key convert-cardano-address-key --shelley-stake-key"
        f" --signing-key-file <(echo {stake_xsk})"
        f" --out-file {file.name}"
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error generating stake skey")
    return


def generate_stake_registration(stake_vkey, file):
    cli_args = [
        "cardano-cli",
        "stake-address",
        "registration-certificate",
        "--stake-verification-key",
        stake_vkey,
        "--out-file",
        file.name,
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error generating registration certificate")
    return


def generate_stake_deregistration(stake_vkey, file):
    cli_args = [
        "cardano-cli",
        "stake-address",
        "deregistration-certificate",
        "--stake-verification-key",
        stake_vkey,
        "--out-file",
        file.name,
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error generating deregistration certificate")
    return


def get_largest_utxo_for_address(address):
    p = subprocess.run(
        [
            "cardano-cli",
            "query",
            "utxo",
            "--out-file",
            "tmp_utxo.json",
            *network_args,
            "--address",
            address,
        ]
    )
    if p.returncode != 0:
        print(p.stderr)
        print("died at get_largest_utxo_for_address")
        raise Exception("Unknown error getting largest UTxO for address")
    f = open("tmp_utxo.json")
    utxo = json.load(f)
    if not utxo:
        print("address has no available utxos")
        exit(1)
    lovelace = 0
    txin = None
    for k, v in utxo.items():
        if len(v["value"]) == 1 and v["value"]["lovelace"] > lovelace:
            lovelace = v["value"]["lovelace"]
            txin = (k, lovelace)
    if txin is None:
        print("No suitable utxo could be found")
        exit(1)
    return txin


def signTx(tx_body, utxo_signing_key, stake_signing_key, out_file):
    cli_args = [
        "cardano-cli",
        "transaction",
        "sign",
        "--tx-body-file",
        tx_body.name,
        "--signing-key-file",
        utxo_signing_key,
        "--signing-key-file",
        stake_signing_key.name,
        "--out-file",
        out_file,
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error signing transaction")
    cli_args = ["cardano-cli", "transaction", "txid", "--tx-file", out_file]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error retrieving txid")
    return p.stdout.rstrip()


def sendTx(out_file):
    cli_args = [
        "cardano-cli",
        "transaction",
        "submit",
        "--tx-file",
        out_file,
        *network_args,
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error sending transaction")


def get_reward_balance(stake_address):
    cli_args = [
        "cardano-cli",
        "query",
        "stake-address-info",
        "--address",
        stake_address,
        *network_args,
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error obtaining reward balance")
    return json.loads(p.stdout.rstrip())[0]["rewardAccountBalance"]


def createRecoveryTx(
    txin,
    stake_xsk,
    stake_vkey,
    stake_address,
    delegation_address,
    rewards,
    change_address,
    payment_signing_key,
    out_file,
):
    with tempfile.NamedTemporaryFile("w+") as stake_dereg_cert, \
         tempfile.NamedTemporaryFile("w+") as stake_reg_cert, \
         tempfile.NamedTemporaryFile("w+") as stake_skey, \
         tempfile.NamedTemporaryFile("w+") as tx_body:

        generate_stake_deregistration(stake_vkey, stake_dereg_cert)
        generate_stake_registration(stake_vkey, stake_reg_cert)
        generate_stake_skey(stake_xsk, stake_skey)

        new_lovelace = (
            # Rich key utxo value in lovelace
            txin[1]

            # Estimated transaction fee (anything higher than the actual tx cost will be given back to the system)
            - 200000

            # Stake rewards which will get paid back to the payment address upon being withdrawn
            + rewards

            # Stake deregistration certificate deposit fee refund
            + 2000000

            # Stake (re)-registration certificate deposit fee payment
            - 2000000
        )

        cli_args = [
            "cardano-cli",
            "transaction",
            "build-raw",
            "--babbage-era",

            # Rich key biggest utxo tx
            "--tx-in",
            txin[0],

            # Tx change back to the payment addr
            "--tx-out",
            f"{change_address}+{new_lovelace}",

            # Withdrawal amount
            "--withdrawal",
            f"{stake_address}+{rewards}",

            # Estimated fee from above
            "--fee",
            "200000",

            # Deregistration certificate
            "--certificate",
            stake_dereg_cert.name,

            # (re-)Registration certificate
            "--certificate",
            stake_reg_cert.name,

            # Tmpfile prior to being signed
            "--out-file",
            tx_body.name,
        ]
        p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stderr)
            print(f"died at tx_body recovery creation: {out_file}")
            raise Exception("Unknown error creating recovery transaction")
        txid = signTx(tx_body, payment_signing_key, stake_skey, out_file)
        return (f"{txid}#0", new_lovelace)


if arguments["--wallet-mnemonic"]:
    with open(arguments["--wallet-mnemonic"], "r") as file:
        mnemonic = file.read().replace("\n", "")
    wallet_root_skey = initialize_root_key(mnemonic)
    wallet_account_skey = derive_account_key(wallet_root_skey, public=False)
    wallet_account_vkey = derive_account_key(wallet_root_skey, public=True)
else:
    print("Must specify wallet mnemonic")
    exit(1)

payment_addr = derive_payment_address_cli_skey(utxo_signing_key)
txin = get_largest_utxo_for_address(payment_addr)
stake_xsk = derive_child_key(wallet_account_skey, f"2/{d_idx}", public=False, chain_code=True)
stake_vkey_ext = derive_child_key(wallet_account_skey, f"2/{d_idx}", public=True, chain_code=True)
stake_vkey = derive_child_key(wallet_account_skey, f"2/{d_idx}", public=True, chain_code=False)
stake_address = derive_stake_address(stake_vkey_ext)
delegation_address = derive_delegation_address(payment_addr, stake_vkey_ext)
rewards = get_reward_balance(stake_address)

print("")
print(f"d_idx = {d_idx}")
print(f"payment_addr = {payment_addr}")
print(f"txin = {txin}")
print(f"stake_address = {stake_address}")
print(f"delegation_address = {delegation_address}")
print(f"rewards = {rewards}")
print("")
print("Building transaction...")
txid = createRecoveryTx(txin, stake_xsk, stake_vkey, stake_address, delegation_address, rewards, payment_addr, utxo_signing_key, f"tx-deleg-account-{d_idx}-restore.txsigned")
print(f"txid = {txid}")
print("")
print("Sending transaction...")
sendTx(f"tx-deleg-account-{d_idx}-restore.txsigned")
exit
