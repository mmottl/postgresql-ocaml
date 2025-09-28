(lang dune 2.7)

(name postgresql)

(generate_opam_files true)

(source
 (github mmottl/postgresql-ocaml))

(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")

(homepage "https://mmottl.github.io/postgresql-ocaml")

(documentation "https://mmottl.github.io/postgresql-ocaml/api")

(maintainers "Markus Mottl <markus.mottl@gmail.com>")

(authors "Alain Frisch <alain.frisch@lexifi.com>"
  "Markus Mottl <markus.mottl@gmail.com>"
  "Petter Urkedal <paurkedal@gmail.com>")

(package
 (name postgresql)
 (synopsis "Bindings to the PostgreSQL library")
 (description
   "Postgresql offers library functions for accessing PostgreSQL databases.")
 (depends
  (ocaml
   (>= 5.00))
  dune-compiledb
  dune-configurator
  (conf-postgresql :build)))
