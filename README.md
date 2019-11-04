# asn1

A asn1 parser library, which can parse public and private keys of RSA and EC formats. 

## Usage

The parser can be run as `lein run {file-path}`.

Three test files are included under resources/keys folder. They can be run by -

    $ lein run resources/keys/ec.pem
    
    $ lein run resources/keys/rsa.keys
    
    $ lein run resources/keys/rsa_public.crt

## Dependencies

The project depends on the following Java8 API - 

1. `java.util.Base64`

`java.util.Base64` is used in place of `javax.xml.bind.DatatypeConverter` for reading the base64 string into bytes. `javax.xml.bind.DatatypeConverter` is not available in Java8. 
