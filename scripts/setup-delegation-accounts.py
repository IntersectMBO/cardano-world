#!/usr/bin/env nix-shell
#!nix-shell -i python -p python3Packages.docopt python3Packages.cbor2

"""distribute

Usage:
    distribute [--testnet-magic INT] [--signing-key-file FILE] [--wallet-mnemonic FILE] [--num-accounts INT]

Options:
    -h --help                    Show this screen
    -t --testnet-magic <INT>     Testnet Magic
    -s --signing-key-file <FILE> Signing Key
    -w --wallet-mnemonic <FILE>  mnemonic file cardano-address uses
    -n --num-accounts <INT>      Number of accounts to setup delegation

"""
#import binascii
import cbor2
import json
import os
import subprocess
import tempfile
from time import sleep
from docopt import docopt
from pathlib import Path

arguments = docopt(__doc__, version='Distribute 0.0')
network_args = []

if arguments["--signing-key-file"] and os.path.exists(arguments["--signing-key-file"]):
  utxo_signing_key = Path(arguments["--signing-key-file"])

else:
  print("Must specify signing key file")
  exit(1)

if arguments["--num-accounts"]:
  num_accounts = int(arguments["--num-accounts"])
else:
  print("Must specify number of accounts")
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
  return derive_child_key(root_key, f"1852H/1815H/0H", public=public)

def derive_payment_address_cli_skey(payment_key_file):
  # TODO: would be nice to just use cardano-addresses
  #with open(payment_key_file, 'r') as file:
  #  cborHex = json.load(file)["cborHex"]
  #   binascii.hexlify(cbor2.loads(binascii.unhexlify(cborHex))).decode(
  #              "ascii"
  #          )
  with tempfile.NamedTemporaryFile("w+") as payment_vkey:
    cli_args = [
        "cardano-cli",
        "key",
        "verification-key",
        "--signing-key-file",
        payment_key_file,
        "--verification-key-file",
        payment_vkey.name

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
        *network_args
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception("Unknown error converting mnemonic to root key")
    return p.stdout.rstrip()

# TODO: this doesn't work for mainnet
def derive_stake_address(stake):
  cli_args = [
      "cardano-address",
      "address",
      "stake",
      "--network-tag",
      "testnet"
  ]
  p = subprocess.run(cli_args, input=stake, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error deriving account key from root key")
  return p.stdout.rstrip()

def derive_payment_address(payment):
  cli_args = [
      "cardano-address",
      "address",
      "payment",
      "--network-tag",
      "testnet"
  ]
  p = subprocess.run(cli_args, input=payment, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error deriving account key from root key")
  return p.stdout.rstrip()

def derive_delegation_address(payment_address, stake_vkey):
  cli_args = [
      "cardano-address",
      "address",
      "delegation",
      stake_vkey
  ]
  p = subprocess.run(cli_args, input=payment_address, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error deriving account key from root key")
  return p.stdout.rstrip()

def derive_child_key(key, derivation, public=False, chain_code=True):
  if chain_code:
    chain_code_arg = "--with-chain-code"
  else:
    chain_code_arg = "--without-chain-code"
  cli_args = [
      "cardano-address",
      "key",
      "child",
      derivation
  ]
  p = subprocess.run(cli_args, input=key, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error deriving account key from root key")
  skey = p.stdout.rstrip()
  if public:
    cli_args = [
        "cardano-address",
        "key",
        "public",
        chain_code_arg
    ]
    p = subprocess.run(cli_args, input=skey, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        raise Exception(f"Unknown error retrieving public key")
    return p.stdout.rstrip()
  else:
    return skey

def generateStakeRegistration(stake_vkey, file):
  cli_args = [
      "cardano-cli",
      "stake-address",
      "registration-certificate",
      "--stake-verification-key",
      stake_vkey,
      "--out-file",
      file.name
  ]
  p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error generating registration certificate")
  return

def createTx(txin, stake_vkey, delegation_address, change_address, payment_signing_key, out_file, delegation_amount=1000000000000):
  with tempfile.NamedTemporaryFile("w+") as stake_reg_cert, tempfile.NamedTemporaryFile("w+") as tx_body:
    generateStakeRegistration(stake_vkey, stake_reg_cert)
    new_lovelace = txin[1] - 2000000 - 200000
    cli_args = [
        "cardano-cli",
        "transaction",
        "build-raw",
        "--babbage-era",
        "--out-file",
        tx_body.name,
        "--tx-in",
        txin[0],
        "--tx-out",
        f"{change_address}+{new_lovelace}",
        "--tx-out",
        f"{delegation_address}+{delegation_amount}",
        "--certificate",
        stake_reg_cert.name,
        *network_args
    ]
    p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
    if p.returncode != 0:
        print(p.stderr)
        print(f"died at tx file: {out_file}")
        raise Exception(f"Unknown error creating transaction")
    txid = signTx(tx_body, payment_signing_key, out_file)
    return (f"{txid}#0", new_lovelace)

def getLargestUtxoForAddress(address):
    p = subprocess.run(["cardano-cli", "query", "utxo", "--out-file", "tmp_utxo.json", *network_args, "--address", address])
    f = open("tmp_utxo.json")
    utxo = json.load(f)
    if not utxo:
      print("address has no available utxos")
      exit(1)
    lovelace = 0
    txin = None
    for k,v in utxo.items():
      if(len(v['value']) == 1 and v['value']['lovelace'] > lovelace):
        lovelace =v['value']['lovelace']
        txin = (k,lovelace)
    if txin == None:
      print("No suitable utxo could be found")
      exit(1)
    return txin

def signTx(tx_body, utxo_signing_key, out_file):
  cli_args = [ "cardano-cli", "transaction", "sign", "--tx-body-file", tx_body.name, "--out-file", out_file, "--signing-key-file", utxo_signing_key]
  p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error signing transaction")
  cli_args = ["cardano-cli", "transaction", "txid", "--tx-file", out_file]
  p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error retrieving txid")
  return p.stdout.rstrip()

def sendTx(out_file):
  cli_args = [ "cardano-cli", "transaction", "submit", "--tx-file", out_file, *network_args]
  p = subprocess.run(cli_args, input=None, capture_output=True, text=True)
  if p.returncode != 0:
      print(p.stderr)
      raise Exception(f"Unknown error sending transaction")


if arguments["--wallet-mnemonic"]:
  with open(arguments["--wallet-mnemonic"], 'r') as file:
    mnemonic = file.read().replace('\n', '')
  wallet_root_skey = initialize_root_key(mnemonic)
  wallet_account_vkey = derive_account_key(wallet_root_skey, public=False)
  wallet_account_skey = derive_account_key(wallet_root_skey, public=True)
else:
  print("Must specify wallet mnemonic")
  exit(1)

payment_addr = derive_payment_address_cli_skey(utxo_signing_key)

txin = getLargestUtxoForAddress(payment_addr)

for i in range(2, num_accounts):
  with tempfile.NamedTemporaryFile("w+") as registration_cert:
    stake_vkey_ext = derive_child_key(wallet_account_vkey, f"2/{i}", public=True, chain_code=True)
    stake_vkey = derive_child_key(wallet_account_vkey, f"2/{i}", public=True, chain_code=False)
    stake_address = derive_stake_address(stake_vkey_ext)
    delegation_address = derive_delegation_address(payment_addr, stake_vkey_ext)
    txin = createTx(txin, stake_vkey, delegation_address, payment_addr, utxo_signing_key,f"tx-deleg-account-{i}.txsigned")
    print(f"setting up delegation for {i}")
    sendTx(f"tx-deleg-account-{i}.txsigned")
