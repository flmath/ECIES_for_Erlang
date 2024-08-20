#!/bin/bash
cd $(dirname "$0")/..

#cat logs/erlang.log 
source scripts/decipher_key_file.sh
source scripts/decrypt_message.sh


awk -f scripts/process_log.awk logs/erlang.log

