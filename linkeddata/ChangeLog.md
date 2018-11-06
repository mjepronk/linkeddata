# Changelog for `linkeddata`

## 0.1.0.0

- Added `ToRDF` and improved `FromRDF`.
- Improved the basic query function `select`.
- Improved the functionality to check if graphs are isomorphic.
- Added `Namespaces` module which contains common prefixes and IRI's.

## 0.0.2.0

- Implemented better IRI handling.
- Turtle and N-Triples parser are based on Attoparsec and streaming now using
  the 'Streaming' library.
- Turtle and N-Triples serialiser are streaming now using the 'Streaming' library.
- Improved test suite.

## 0.0.1.0

- Basic datatypes and their arbitrary instances defined, both heavily influenced
  by rdf4h.
- IRI datatypes and parser.
- Turtle parser and serialiser and N-Triples parser and serialiser implemented.
- Isomorphism algorithm from Aidan Hogan's paper implemented.
- Basic query functionality implemented.
- Test files from W3C added.
