PostgreSQL-OCaml - PostgreSQL Bindings for OCaml
================================================
                                        
---------------------------------------------------------------------------

What is PostgreSQL-OCaml?
-------------------------
  
This [OCaml](http://www.ocaml.org)-library provides an interface to
[PostgreSQL](http://www.postgresql.org), an efficient and reliable, open
source, relational database.  Almost all functionality available through
the C-API (`libpq`) is replicated in a type-safe way.  This library uses
objects for representing database connections and results of queries.

Usage
-----

The OCaml-API in file `lib/postgresql.mli` is well-documented and can be
built as HTML with `make doc`.  The API-documentation can also be found
[online](http://mmottl.github.io/postgresql-ocaml/api).

More detailed information on how to interact with PostgreSQL is available
in the [PostgreSQL-documentation](http://www.postgresql.org/docs).
The OCaml-examples in the `examples`-directory are mostly very short and
comprehensible and therefore a good way to get started.

---------------------------------------------------------------------------

Contact Information and Contributing
------------------------------------

In the case of bugs, feature requests, contributions and similar, please
contact the maintainers:

  * Markus Mottl <markus.mottl@gmail.com>
  * Alain Frisch <alain.frisch@lexifi.com>

Up-to-date information should be available at:
<http://mmottl.github.io/postgresql-ocaml>

Enjoy!

Markus Mottl on July 10, 2012
