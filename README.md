ECIES for Erlang
=====

Based OTP application with two state gen_statem, proof of concept for using asymeric file encryption with openssl, OTP crypto and OTP public_key.

## Problem description:

We have a public facing Erlang server that has to log some sensitive clients information (in example GDPR, HIPAA or GLBA protected). 
In case of a hostile agent get access to the server we do not want it to read our clients data.
Unfortunatelly public_key:encrypt_private/2(https://www.erlang.org/doc/apps/public_key/public_key.html#encrypt_private/2) function and other similar are deprecated in OTP. We can use sign and verify, but those functions guarantee only integrity and not confidentiality.

## Solution:

We create a public key cryptosystem. The public key is used by the server to encrypt sensitive records. The private key is stored in air-gapped server in case we need to read those data from the logs. We create our own Elliptic Curve Integrated Encryption Scheme (ECIES), based on https://flmath.github.io/posts/jupyter/AsymetricCrypto.

## Steps:

1. Use openssl to create a private/public key pair.
2. Store the private key (PvKI) in an isolated machine and the public key PuKI kept on server.
3. Create an ephermal private server key (PvSK) and connected public key (PuSK) withing erlang server. Use crypto:generate_key/2(https://www.erlang.org/doc/apps/crypto/crypto.html#generate_key/2) 
4. When sensitive data has to be logged the data is encrypted with a secret created from
PvSK and PuKI plus additional data like Initiation Vector, Salt etc are used to encrypt information. The PuSK, IV, Salt, but not PvSK need to be provided in the logs.
Use crypto:crypto_one_time/5(https://www.erlang.org/doc/apps/crypto/crypto.html#crypto_one_time/5)
5. The PvSK should be deleted and regenerated after encryption or at least every specified time.
6. If there is needs to decrypt data, it should be copied to the isolated machine, and there logged PuSK, IV, Salt, and PvKI can be used to recreate shared secret and decrypt sensitive information.
Use for that:
openssl pkeyutl -derive -inkey PvKI -peerkey PuSK -out secret
openssl enc -d -base64

Details in the code.

## Test solution:

Clean, just in case.
-----
scripts/clean.sh 

Build
-----
    Install erlang and rebar3 for example with asdf.
    https://github.com/asdf-vm/asdf
    https://github.com/asdf-vm/asdf-erlang
    https://github.com/Stratus3D/asdf-rebar
    
    $ rebar3 compile

Create the Isolated Server Keys
-----
scripts/generate.sh 

Run application in a shell and switch state to enrypted for a few seconds
-----
        
    $ rebar3 shell
    > tracing_experiments:switch_state().
    ...
    > tracing_experiments:switch_state().
    > q().

The unencrypted and encrypted records will be added to logs/erlang.log
-----
cat logs/erlang.log

2024-08-20T07:49:27.333830+02:00 notice: unecrypted enter_encrypted_statesensitive state 0
2024-08-20T07:49:27.336480+02:00 notice: encrypted 652ec1980fbb9afed915fd9075f3fb95b67ea5187c03a62a5139edbabc384993027a454e48ac0e4df248deeb685852ff11144901197d26df0e93bbc1ec5763ce b0bd2dfe4391db1102ef09b346e0e70b e7NgjqwZnmFEiYiN7EF0AMZiGFub/SKP0Jikiaus3Mf7Q00i/c4=

Now the logs can be decrypted for record marked as encrypted and compared.
-----

2024-08-20T07:49:27.333830+02:00 notice: unencrypted enter_encrypted_state sensitive state 0
File created: isolated/decryption_keys/652ec1980fbb9afed915fd9075f3fb95b67ea5187c03a62a5139edbabc384993027a454e48ac0e4df248deeb685852ff11144901197d26df0e93bbc1ec5763ce.sharedhex

14c0081fc339b8ced2099716e35663407b3699f5108fce60977ad1c2183f4e982024-08-20T07:49:27.336480+02:00 notice: decrypted enter_encrypted_state sensitive state 0


-----
Done!