#!/bin/awk -f

function create_key(filename) {
    cmd = "scripts/decipher_key_file.sh " filename
    return system(cmd)
}

function decrypt_message(timestamp, level, filename, salt, iv, message) {
    cmd = "scripts/decrypt_message.sh " timestamp " " level " " \
    filename " " salt " " iv " " message
    return system(cmd)
}

BEGIN {
    FS =" "           # Set field separator to comma
    total = 0
}

{
    if(  $3 ~ /encrypted.*/){
	total += 1        # Add value from the second column
	create_key($4)
	decrypt_message($1,$2,$4,$5,$6,$7)

    }
    else
    	print $0
  
}

END {
        print "Decrypted lines: " total
 
}
