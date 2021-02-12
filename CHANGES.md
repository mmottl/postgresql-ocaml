### 5.0.0 (2021-02-12)

  * Added support for parameter types to `exec`, `prepare`, `send_query`,
    and `send_prepare`.

    Thanks to Petter A. Urkedal for the patch!


### 4.6.3 (2020-08-15)

  * Removed incorrect `[@@noalloc]` from `is_busy` external call.

    Thanks to Dmitry Astapov for this patch!


### 4.6.2 (2020-08-04)

  * Removed `base` and `stdio` build dependencies.


### 4.6.1 (2020-07-29)

  * Fixed a bug in `request_cancel` that turned errors into success and
    success into an error.  Thanks to Dmitry Astapov for this patch!

  * Added support for const char strings in stubs due to stricter handling
    in newer OCaml runtimes.  This eliminates C-compiler warnings.


### 4.6.0 (2020-05-22)

  * Fixed missing runtime release during calls to PQisBusy.

  * Added a temporary workaround for dealing with notice processing and
    asynchronous operations.

    Thanks to Petter A. Urkedal for the patch!


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
