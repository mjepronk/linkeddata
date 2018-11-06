# Linked Data for Haskell

This repository contains several packages:

- [`linkeddata`](linkeddata/README.md): base library with utilities for working with RDF and Linked Data.
- Soon: `linkeddata-sparql`: a client to perform requests against a SPARQL endpoint.
- Soon: `linkeddata-sparql-engine`: an engine to perform SPARQL queries against a local graph.

## TODO

- Finish the SPARQL client.
- Finish the SPARQL engine.
- Pass the full W3C test suite for Turtle and N-Triples format.
- Provide (de-)serialisation of the JSON-LD format.
- Provide (de-)serialisation of the RDF/XML format.

## Comparison with rdf4h and hsparql

Here I provide a very incomplete comparison of `linkeddata` with
[`rdf4h`](https://github.com/robstewart57/rdf4h) and
[`hsparql`](https://github.com/robstewart57/hsparql):

- `linkeddata` is much younger and for now has a less stable API than `rdf4h`.
  There may be more bugs and performance issues. For now, `rdf4h` and/or
  `hsparql` are probably a safer bet.

- `linkeddata`s parsers are streaming, which gives us constant memory usage in
  most cases, even for very large graphs.

- `linkeddata` provides the classes `FromRDF` and `ToRDF` to convert your
  Haskell datatypes to RDF literals.

- `linkeddata` uses type safe IRI's.

- Soon: `linkeddata-sparql` shares types with the base `linkeddata` library.

- Soon: `linkeddata-sparql-engine` provides a SPARQL engine which allows us to
  query our local graphs using SPARQL 1.1 queries.

- Unlike `rdf4h`, `linkeddata` does not provide a parser for RDF/XML yet.

- Unlike `rdf4h`, `linkeddata` does not yet pass all W3C tests.

## Sources

- [RDF 1.1 Semantics](https://www.w3.org/TR/rdf11-mt/)
- [RDF 1.1 Concepts and Abstract Syntax](https://www.w3.org/TR/rdf11-concepts/)
- [RDF Schema 1.1](https://www.w3.org/TR/rdf-schema/)
- [RDF 1.1 Turtle](https://www.w3.org/TR/turtle/)
- [RDF 1.1 N-Triples](https://www.w3.org/TR/n-triples/)
- [JSON-LD 1.0](https://www.w3.org/TR/json-ld/)
- [SPARQL 1.1 Overview](https://www.w3.org/TR/sparql11-overview/)
- [RFC3987 Internationalized Resource Identifiers (IRIs)](https://tools.ietf.org/html/rfc3987)

## License

`linkeddata`, `linkeddata-sparql` and `linkeddata-sparql-engine` are BSD3
licensed.
