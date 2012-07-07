      
                                        
                      README: library "PostgreSQL-OCaml"
                      **********************************
                  Copyright   (C)   2008  Markus Mottl (1)  
                  ==========================================
                          Vienna, November 29, 2008
                          =========================
  

1  Directory contents
*=*=*=*=*=*=*=*=*=*=*

   
                                        
   -----------------------------------------------------------------------
   |        Changes          |           History of code changes         |
   -----------------------------------------------------------------------
   |        INSTALL          |        Short notes on compiling and       |
   |                         |           installing the library          |
   -----------------------------------------------------------------------
   |        LICENSE          |     "GNU LESSER GENERAL PUBLIC LICENSE"   |
   -----------------------------------------------------------------------
   |        Makefile         |                Top Makefile               |
   -----------------------------------------------------------------------
   |     OCamlMakefile       |        Makefile for easy handling of      |
   |                         |         compilation of not so easy        |
   |                         |  OCaml-projects. It generates dependencies|
   |                         |        of OCaml-files automatically,      |
   |                         |       is able to handle "ocamllex"-,      |
   |                         |     "ocamlyacc"-, IDL- and C-files and    |
   |                         |       generates native- or byte-code      |
   |                         |        as executable or as library -      |
   |                         |      with thread-support if you want!     |
   -----------------------------------------------------------------------
   |       README.txt        |                  This file                |
   -----------------------------------------------------------------------
   |     examples/dump/      |           Dumps a table to stdout         |
   -----------------------------------------------------------------------
   |   examples/populate/    |         Inverse operation of "dump"       |
   -----------------------------------------------------------------------
   |    examples/prompt/     |        A simple replacement for psql      |
   -----------------------------------------------------------------------
   |  examples/prompt_gtk/   |       Graphical version of "prompt".      |
   |                         |             Requires lablgtk!!!           |
   -----------------------------------------------------------------------
   |   examples/test_lo/     |   Demonstrates Large Objects manipulation |
   -----------------------------------------------------------------------
   |          lib/           |        OCaml-library for interfacing      |
   |                         |           the PostgreSQL-database         |
   -----------------------------------------------------------------------
                                        
  
  

2  What is the "PostgreSQL-OCaml"-library?
*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

  
  This OCaml-library provides an interface to PostgreSQL (tested with version
7.0.2, 7.1.3 and 7.4.1), an efficient and reliable, open source, relational
database. Almost all functionality available through the C-API (libpq) is
replicated in a type-safe way. This library uses objects for representing
database connections and results of queries.
  

3  How can you use it?
*=*=*=*=*=*=*=*=*=*=*=

  
  The interface is well-documented, and more detailed information on how to
interact with PostgreSQL is available from the PostgreSQL-website (2). The
examples in the examples-directory are mostly very short and comprehensible
and therefore a good way to get started.
  

4  Contact information
*=*=*=*=*=*=*=*=*=*=*=

  
  In the case of bugs, feature requests and similar, you can contact me here:
  
     markus.mottl@gmail.com
  
   Up-to-date information concerning this library should be available here:
  
     http://www.ocaml.info/ocaml_sources
  
   Enjoy!!
  
   
-----------------------------------------------------------------------------
  
   This document was translated from LaTeX by HeVeA (3).
--------------------------------------
  
  
 (1) http://www.ocaml.info/
 
 (2) http://www.postgresql.org
 
 (3) http://hevea.inria.fr/index.html
