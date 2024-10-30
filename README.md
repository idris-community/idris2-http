# idris2-http

This is a http 1.1 client library written in idris2.

# Features
Currently, the library supports the following features:
- Reusage of keep-alive connections.
- GZip (RFC 1952) / Zlib Deflate (RFC 1950) decompression.
- TLS (https)
- Incremental sending and receiving via [stream](src/Utils/Streaming.idr)

# Examples
An example on how it can be used can be found in [here](tests/src/ClientTest.idr).

# Installation
The latest version of this library can be installed with [pack](https://github.com/stefan-hoeck/idris2-pack).
