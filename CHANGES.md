### 4.5.2 (2019-10-28)

  * Switched from `caml_alloc_custom` to `caml_alloc_custom_mem`.

    This should improve memory usage and GC performance.

  * Switched to OPAM file generation via `dune-project`


### 4.5.1 (2019-10-11)

  * Fixed warnings in C-stubs

  * Support detection of release candidate version numbers


### 4.5.0 (2019-06-06)

  * Added support for `put_copy_data`, `put_copy_end`, and `get_copy_data`

  Thanks to Petter A. Urkedal for the patch!


### 4.4.2 (2019-03-28)

  * Fixed documentation formatting warnings

  * Fixed OpenSUSE depexts


### 4.4.1 (2018-10-25)

  * Switched to dune, dune-release, and OPAM 2.0


### 4.4.0 (2018-07-08)

  * Added support for executing queries with binary results.

    Thanks to Paul Biggar for the patch!


### 4.3.0 (2017-12-30)

  * Added error handling functions for extracting more error details.

    Thanks to Sean Grove for the patches!


### 4.2.1 (2017-11-22)

  * Improved finalization of result values for better performance.


### 4.2.0 (2017-10-10)

  * Fixed bigarray library dependencies.  No need for the user to explicitly
    link with `bigarray` when using certain functions anymore.

  * Used untagged integer representations in external calls for improved
    efficiency.


### 4.1.0 (2017-08-02)

  * Switched to jbuilder and topkg
