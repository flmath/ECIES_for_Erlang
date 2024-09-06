#!/bin/bash
cd $(dirname "$0")/..

decrypt_message() {
   # echo "para"
   # echo "$3 $4 $5 $6 $1 $2"

    
    local iv="$3"
    local msg="$4"
    local salt="$2"
    local pass=$(cat isolated/decryption_keys/${1}.sharedhex)
    local passkdf=$(openssl kdf -keylen 32 -kdfopt digest:SHA256 -kdfopt hexsalt:${salt} -kdfopt hexpass:${pass} -kdfopt iter:1 PBKDF2 | tr -d ':')
    echo "${msg}" | openssl enc -d -base64 -aes-256-ctr \
			    -iv "${iv}" \
			    -K "${passkdf}" 2>/dev/null | \
	xargs -I {} echo $5 $6 "decrypted "{}
}
#filename iv message

decrypt_message $3 $4 $5 $6 $1 $2



