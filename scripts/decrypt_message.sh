#!/bin/bash
cd $(dirname "$0")/..

decrypt_message() {
    local iv="$2"
    local msg="$3"
    local pass=$(cat isolated/decryption_keys/${1}.sharedhex)

    echo "${msg}" | openssl enc -d -base64 -aes-256-ctr -iv "${iv}"  -nopad  -nosalt \
		      -K "${pass}" 2>/dev/null | xargs -I {} echo $4 $5 "decrypted "{}
}
#filename iv message
decrypt_message $3 $4 $5 $1 $2
