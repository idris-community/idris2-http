# idris2-http

This is a http 1.1 client library written in idris2.

# Compiler version
The project currently targets Idris2 compiler after the commit [768d164e](https://github.com/idris-lang/Idris2/commit/768d164ec9cf37638f7604ee3a22de2aabfc6852).

# Features
Currently, the library supports the following features:
- Reusage of keep-alive connections.
- GZip (RFC 1952) / Zlib Deflate (RFC 1950) decompression.
- TLS (https)
- Incremental sending and receiving via [stream](src/Utils/Streaming.idr)

# Examples
An example on how it can be used can be found in [here](tests/src/ClientTest.idr).

# Dependencies
This library depends on my [idris2-tls](https://github.com/octeep/idris2-tls) library and Stefan Hoeck's [idris2-sop](https://github.com/stefan-hoeck/idris2-sop) library.
