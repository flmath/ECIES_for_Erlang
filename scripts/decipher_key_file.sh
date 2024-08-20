#!/bin/bash
cd $(dirname "$0")/..

decipher_key_file() {
    local file="$1"
    if [ -f "isolated/decryption_keys/${file}.sharedhex" ]; then
#	echo "File exists: isolated/decryption_keys/${file}.sharedhex"
        return 0
    else
	cat scripts/asn1/public_key_brainpoolP256r1.asn | \
	    sed "s/insert_here/04${file}/" > "isolated/decryption_keys/${file}.asn"
	openssl asn1parse -genconf "isolated/decryption_keys/${file}.asn" \
		-out "isolated/decryption_keys/${file}.der"
	openssl pkeyutl -derive -inkey isolated/private_key.pem \
		-peerkey "isolated/decryption_keys/${file}.der" \
		-out "isolated/decryption_keys/${file}.shared"
	cat isolated/decryption_keys/${file}.shared \
	    | xxd -p | tr -d '\n' > isolated/decryption_keys/${file}.sharedhex
							  
	echo "File created: isolated/decryption_keys/${file}.sharedhex"
	cat isolated/decryption_keys/${file}.sharedhex
        return 0
    fi
}
decipher_key_file $1
