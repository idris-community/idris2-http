# idris2-http

This is a http 1.1 client library written in idris2.

# Support
Currently, the library supports the following features:
- Reusage of keep-alive connections.
- GZip / Deflate decompression.
- TLS (https)
- Incremental sending and receiving (this is accomplished with stream)

# Examples
An example on how it can be used can be found in [here](tests/src/ClientTest.idr).

# Dependencies
This library depends on my [idris2-tls](https://github.com/octeep/idris2-tls) library and Stefan Hoeck's [idris2-sop](https://github.com/stefan-hoeck/idris2-sop) library.
