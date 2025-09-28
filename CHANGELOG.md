# Changelog

## [5.3.2] - 2025-09-27

### Fixed

- Enable binary results for parameterless queries by calling `PQsendQueryParams`
  when `binary_result` is requested (instead of `PQsendQuery`). Thanks to Alain
  Frisch for the patch.

## [5.3.1] - 2025-09-23

### Fixed

- `getvalue` and `get_escaped_value` now return `Postgresql.null` for SQL NULL
  values, matching the documentation. Thanks to Christophe Raffalli for the
  patch.

### Changed

- Applied OCaml/C formatting and added an ocamlformat pre-commit hook.

## [5.3.0] - 2025-09-23

### Added

- `Connection` functor that allows for custom mutex implementations. This
  improves compatibility with effect-based concurrency frameworks and helps
  prevent deadlocks.
- Makefile target to generate `compile_commands.json` for improved LSP support
  in editors.
- Version discovery supports PostgreSQL beta/rc versions (via `pg_config` and
  `pkg-config`). Thanks to Antonio Nuno Monteiro for the patch.

### Changed

- Improved thread coordination during cancellation of operations.
- Removed the use of C stubs to check for a finished connection.
- Replaced custom resource management logic with `Fun.protect`.

Thanks to Christophe Raffalli for contributing solutions for the improved thread
coordination and for effect-based concurrency frameworks.

## [5.2.0] - 2025-06-20

### Added

- `binary_result` to `send_query` and `send_query_prepared`. Thanks to
  Christophe Raffalli for the contribution.
- `pre-commit` configuration and GitHub workflow.

## [5.1.3] - 2024-12-08

### Fixed

- Cross-compilation in discover process. Thanks to Antonio Nuno Monteiro for the
  patch.

## [5.1.2] - 2024-12-08

### Fixed

- Version discovery with pkg-config.
- Macro naming and instantiation formatting.
- Copyright notices.

### Removed

- Obsolete `base-bytes` dependency.

### Changed

- Switched to ocamlformat 0.27.0.
- Improved GitHub workflow.

## [5.1.1] - 2024-11-26

### Fixed

- Rare, architecture-specific GC bug in `lo_seek`.

## [5.1.0] - 2024-11-04

### Added

- GitHub workflows.
- Automatic formatting with ocamlformat and clang-format.

### Fixed

- License format and a typo.
- Some odoc references.

### Changed

- Made `Postgresql.null` a now unique, empty string.
- Ported the config discovery script to pkg-config. Thanks to Antonio Nuno
  Monteiro for the patch.
- Improved Dune rules.
- Used new OCaml 4.12 C-macros.
- Switched to Dune lang 2.7.

## [5.0.0] - 2021-02-12

### Added

- Support for parameter types to `exec`, `prepare`, `send_query`, and
  `send_prepare`. Thanks to Petter A. Urkedal for the patch.

## [4.6.3] - 2020-08-15

### Removed

- Incorrect `[@@noalloc]` from `is_busy` external call. Thanks to Dmitry Astapov
  for this patch.

## [4.6.2] - 2020-08-04

### Removed

- `base` and `stdio` build dependencies.

## [4.6.1] - 2020-07-29

### Fixed

- Bug in `request_cancel` that turned errors into success and success into an
  error. Thanks to Dmitry Astapov for this patch.

### Added

- Support for const char strings in stubs due to stricter handling in newer
  OCaml runtimes, eliminating C-compiler warnings.

## [4.6.0] - 2020-05-22

### Fixed

- Missing runtime release during calls to PQisBusy.

### Added

- Temporary workaround for dealing with notice processing and asynchronous
  operations. Thanks to Petter A. Urkedal for the patch.

## [4.5.2] - 2019-10-28

### Changed

- Switched from `caml_alloc_custom` to `caml_alloc_custom_mem` to improve memory
  usage and GC performance.
- Switched to OPAM file generation via `dune-project`.

## [4.5.1] - 2019-10-11

### Fixed

- Warnings in C-stubs.

### Added

- Support detection of release candidate version numbers.

## [4.5.0] - 2019-06-06

### Added

- Support for `put_copy_data`, `put_copy_end`, and `get_copy_data`. Thanks to
  Petter A. Urkedal for the patch.

## [4.4.2] - 2019-03-28

### Fixed

- Documentation formatting warnings.
- OpenSUSE depexts.

## [4.4.1] - 2018-10-25

### Changed

- Switched to dune, dune-release, and OPAM 2.0.

## [4.4.0] - 2018-07-08

### Added

- Support for executing queries with binary results. Thanks to Paul Biggar for
  the patch.

## [4.3.0] - 2017-12-30

### Added

- Error handling functions for extracting more error details. Thanks to Sean
  Grove for the patches.

## [4.2.1] - 2017-11-22

### Improved

- Finalization of result values for better performance.

## [4.2.0] - 2017-10-10

### Fixed

- Bigarray library dependencies, eliminating the need for explicit linking with
  `bigarray`.

### Changed

- Used untagged integer representations in external calls for improved
  efficiency.

## [4.1.0] - 2017-08-02

### Changed

- Switched to jbuilder and topkg.

## Changes Before Version 4.1.0

```text
2016-02-12:  Fixed GTK-example to make it compile again.

             Thanks to Jonathan Curran for the patch.

2016-02-11:  Improved the notification API (conn#notifies).

             Thanks to Jonathan Curran for the initial patch.

2015-12-18:  Fixed a GC-bug when unescaping with "unescape_bytea".

             Thanks to Sebastien Mondet for the bug report and for testing
             the patch.

2015-07-10:  Fixed a GC-bug in PQconndefaults_stub.

             Thanks to Roven Gabriel for the patch.

2015-03-27:  Added methods ftype_oid and paramtype_oid and fixed a minor bug.

             Thanks to Tomohiro Matsuyama for the patches.

2015-03-26:  Added support for JSON and JSONB field types.

             Thanks to Tomohiro Matsuyama for the patch.

2015-01-28:  New major release: improved handling of asynchronous operations.
             This required an API-change to the "flush" method.

             Thanks to Max Wolter from Jane Street Capital for the patch.

2014-10-29:  Fixed an installation problem due to a missing internal module.

2014-10-23:  Fixed string handling for new OCaml version 4.02 (String/Bytes
             modules). Requires new findlib version (>= 1.5).

2014-08-14:  Even more new asynchronous methods:

               * optional [startonly] flag for creating asynchronous connections
               * connect_poll
               * reset_start
               * reset_poll

             And also:

               * set_single_row_mode

             Thanks to Petter Urkedal <paurkedal@gmail.com> for these
             contributions.

2014-08-10:  New asynchronous methods:

               * send_describe_prepared
               * send_describe_portal

             Thanks to Petter Urkedal <paurkedal@gmail.com> for these
             contributions.

2014-03-10:  New asynchronous methods:

               * send_prepare
               * send_query_prepared

             Thanks to Petter Urkedal <paurkedal@gmail.com> for these
             contributions.

2013-07-08:  Fixed a version discovery problem with beta versions of
             PostgreSQL.

             Thanks to Stephane Legrand <stephleg@free.fr> for the patch.

2012-07-20:  Downgraded findlib version constraint to support the Debian
             testing branch.

2012-07-15:  New major release version 2.0.0:

               * Upgraded to OCaml 4.00
               * Switched to Oasis for packaging
               * Switched to OCamlBuild for the build process
               * Rewrote README in Markdown
               * Added stricter compilation flags
               * Fixed broken copy_out method

2012-01-10:  Added new function:

               * Postgresql.get_escaped_value

             Thanks to Jonathan Derque <jonathan.derque@lexifi.com> for the
             patch.

2011-12-29:  Fixed a memory allocation bug getting values of binary format.

             Thanks to Igor Plotnikov <igor@xambala.com> for the patch.

2011-05-23:  Fixed conndefaults bindings some more due to unclear PostgreSQL
             documentation.

             Thanks to Vijai Lulla <vijaylulla@gmail.com> for the bug report.

2011-05-21:  Fixed GC bug in conndefaults function.

             Thanks to Vijai Lulla <vijaylulla@gmail.com> for the bug report.

2010-12-25:  Added support for the new bytea hex format in PostgreSQL 9.0.

             Thanks to Alain Frisch <alain@frisch.fr> for the initial patch.

2010-12-08:  Fixed findlib linking problem with bigarrays.

2010-10-17:  Added better support for prepared statements.

             Added support for zero-copy I/O with large objects.

             Thanks to Chris King <colanderman@gmail.com> for the above two
             patches.

             Improved handling of sizes in C-bindings.

2010-03-17:  Fixed small copy_out bug.

2010-02-17:  Signal an error at link time if multi-threading support is
             not available.

             Thanks to Guillaume Yziquel <guillaume.yziquel@citycable.ch>
             for the hint.

2009-10-12:  Fixed example build problem.

2009-10-07:  Minor API change.

             "socket" method now returns an integer instead of a
             Unix file descriptor to avoid type problems under
             Windows.

2009-09-18:  Fixed serious bug in new escape_string method.

             PLEASE UPGRADE

2009-09-08:  API-change: deleted "escape_string" function.

             There is now a method "escape_string" in the connection
             class, which is not deprecated and hence safer.

2009-05-07:  Significant improvements to thread-safety.

2009-01-16:  Added missing null parameter.

             Thanks to Alain Frisch <alain@frisch.fr> for the patch.

2009-01-15:  Addes support for specifying binary parameters in queries.

             Thanks to Alain Frisch <alain@frisch.fr> for the patch.

2009-01-05:  Switched to generational global root registration of
             callbacks for better performance.

             Requires OCaml 3.11 or higher.

2008-10-25:  Fixed portability problem on Mac OS X.

2008-09-30:  Fixed bug that prevented use with PostgreSQL 8.1.

2008-09-29:  Added support for connection methods:

               * describe_prepared

             Added support for result methods:

               * nparams
               * paramtype

             Thanks to Paolo Donadeo <p.donadeo@ex-nunc.org> for the
             above contributions.

             Updated OCamlMakefile.

2008-03-19:  Allow commands in one query again (broken after adding support for
             query parameters). Updated the INSTALL file with a hint on how
             to solve a potential linking problem on Windows.

             Thanks to Alain Frisch <alain@frisch.fr> for this patch
             and hint.

2008-03-14:  Merged with Jane Street Capital version (some minor
             cleanups).

2008-03-10:  Improved portability to Windows. Added support for
             parameters in queries. Some internal cleanups.

             Thanks to Alain Frisch <alain@frisch.fr> for this patch.

2007-03-28:  Greatly improved multi-thread support.

             Added seek options.

             Improved documentation.

             Updated OCamlMakefile.

2007-03-28:  Fixed a potential GC-bug. Updated OCamlMakefile.

2007-03-19:  Updated OCamlMakefile.

2007-03-19:  Fixed a linking problem on Mac OS X.

             Thanks to Leonardo Cecchi <leonardo.cecchi@gmail.com>
             for the hint.

2007-01-08:  Fixed a build problem related to a bug in OCamlMakefile.

             Thanks to Anastasia Gornostaeva <ermine@ermine.pp.ru>
             for the hint.

2007-01-08:  Fixed a build problem with non-standard locations of PostgreSQL.

             Thanks to Anastasia Gornostaeva <ermine@ermine.pp.ru>
             for the patch.

2006-11-22:  Updated OCamlMakefile.

2006-11-08:  Upgraded prompt_gtk to LablGTK2.

             Tightened compilation warnings.

             Removed superfluous linking option.

             Replaced obsolete escaping method, and added calls to
             PQfreemem required on Windows.

2006-09-15:  Updated OCamlMakefile.

2006-08-17:  Automatically fetch compiler and linker flags for the local
             PostgreSQL-installation for simpler installation. Thanks to
             Vincenzo Ciancia <ciancia@di.unipi.it> for the hint.

2006-07-21:  Fixed GC-bug.

2006-06-08:  Fixed GC-bugs.

2006-01-24:  Added support for accessing binary fields.

2005-05-31:  Fixed some uncleanliness reported by Saffire (an FFI-type
             checker).

2005-03-22:  Small internal improvement.

2004-12-28:  Fixed a potential compilation problem.

2004-12-20:  Fixed a bug in the "escape_bytea"-function: escaped strings
             contained an extra null character.

             Thanks to Christophe Troestler
             <Christophe.Troestler@umh.ac.be> for the bug report.

             Updated OCamlMakefile.

2004-08-26:  Small internal code-improvement.

2004-08-05:  Significant improvements: the oid type is now revealed as int in
             the interface. The "ftype"-method now returns the new type
             "ftype". The latter lists 60 constructors, which specify the
             types of fields.

             The change comes with new conversion functions:

               * ftype_of_oid
               * oid_of_ftype
               * string_of_ftype
               * ftype_of_string

             And with new exceptions:

               * exception Oid of oid
               * exception InternalError of string

2004-08-02:  Fixed two serious thread-related bugs.

2004-07-14:  Minor update for OCaml-3.08.

             Updated OCamlMakefile.

2004-07-06:  Added an example "cursor" that demonstrates the use of
             cursors.

2004-06-29:  Fixed an off-by-one error in method "get_all_lst".
             Thanks to Anil Madhavapeddy <anil@recoil.org> for the patch.

             Updated OCamlMakefile.

2004-04-27:  Updated OCamlMakefile.

2004-04-13:  Fixed a problem with backwards compatibility with older
             OCaml-versions that do not yet use the caml-namespace (identifiers
             not yet prefixed with "caml_").

2004-02-21:  Added CAMLprim in the C-interface where appropriate.

2004-02-08:  Fixed minor bugs with returning unit values from C.
             Made C-interface compile with pedantic compiler settings.

2004-01-29:  Some major changes to interface to use or improve handling
             of default arguments. Effected methods:

               * escape_string
               * escape_bytea
               * getline
               * getline_async
               * putnbytes
               * lo_write
               * lo_read
               * lo_seek

             Removed methods (obsolete due to default arguments - use
             "lo_write" instead):

               * lo_write_string

             Added functions:

               * unescape_bytea

2004-01-28:  First release.
```
