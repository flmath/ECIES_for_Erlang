#!/bin/bash
cd $(dirname "$0")/..

mkdir -p certs
mkdir -p isolated
mkdir -p isolated/decryption_keys

# generate on airgapped server curve:
#http://oid-info.com/cgi-bin/display?oid=brainpoolP256r1&action=display
#1.3.36.3.3.2.8.1.1.7

openssl genpkey -algorithm EC -pkeyopt ec_paramgen_curve:brainpoolP256r1\
	-out  isolated/private_key.pem
openssl pkey -pubout -in isolated/private_key.pem\
	-out isolated/public_key.pem
# copy public key for encryption to Public Facing Server, from now one airgapped server
# is off the limits
cp isolated/public_key.pem certs/

