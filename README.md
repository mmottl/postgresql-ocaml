# PostgreSQL-OCaml - PostgreSQL Bindings for OCaml

## Overview

PostgreSQL-OCaml is an [OCaml](http://www.ocaml.org) library that provides a
type-safe interface to [PostgreSQL](http://www.postgresql.org), a reliable and
efficient open-source relational database. It replicates almost all
functionality available through the C-API (`libpq`) using objects to represent
database connections and query results.

## Usage

The OCaml API, detailed in `lib/postgresql.mli`, is well-documented. You can
generate HTML documentation with `make doc`, or access it
[online](http://mmottl.github.io/postgresql-ocaml/api/postgresql).

For more information on interacting with PostgreSQL, refer to the
[PostgreSQL documentation](http://www.postgresql.org/docs). The `examples`
directory contains concise OCaml examples that are ideal for beginners.

## Contributing

To report bugs, request features, or contribute, please use the
[GitHub issue tracker](https://github.com/mmottl/postgresql-ocaml/issues).

For the latest updates, visit: <https://mmottl.github.io/postgresql-ocaml>
