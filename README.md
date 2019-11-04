# asn1

A asn1 parser library, which can parse public and private keys of RSA and EC formats. 

## Usage

The parser can be run as `lein run {file-path}`.

Three test files are included under resources/keys folder. They can be run by -

    $ lein run resources/keys/ec.pem
    
    $ lein run resources/keys/rsa.keys
    
    $ lein run resources/keys/rsa_public.crt

## Dependencies

The project depends on the following Java8 API's - 

1. `java.util.Base64`
2. `java.time`

`java.util.Base64` is used in place of `javax.xml.bind.DatatypeConverter`, which is not available in Java8. 

`java.time` is needed for parsing UTC time values. 

