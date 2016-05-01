#####################################
Types and Programming Languages in F#
#####################################

Code and Examples from Benjamin Pierce's "`Types and Programming Languages`_".

.. _`Types and Programming Languages`: http://www.cis.upenn.edu/~bcpierce/tapl/


Overview
========

"`Types and Programming Languages`_" provides a comprehensive introduction to type systems and programming language theory. The code which accompanies the book is written in OCaml; this repository contains an F# port of that code, using FsLexYacc for lexing and parsing.

**NOTE:** The ported F# code is not a fresh implementation -- it is the *original* OCaml code with some trivial modifications which allow it to compile with F#. The output of each of the F# projects has been verified to match the `output of the original OCaml programs`_.

This is a fork of Jack Pappas' original F# port with the following enhancements:

- Uses the latest nuget release of FsLexYacc
- Isolated Ocaml compatability dependencies to pretty printing
- Linted code and more idiomatic F# for most of the code that is not specific to tapl algorithms
- Enhanced command line, including optional output to file and input from the command line
- A suite of regression tests.

.. _`output of the original OCaml programs`: fsharp-tapl/blob/master/expected-output.rst

Prerequisites
=============

- F# compiler

  - Windows

    - Visual Studio 2013 or later (requires F# 4.0)

  - Mac OS X / FreeBSD / Linux (this fork not yet tested on non-Windows)

    - Mono 3.0 (or newer)
    - `fsharp 4.0`_
    - MonoDevelop 3.0 (or newer)

- Run the ``build.cmd`` to build all the projects and run tests.
  
  On a related note, Jack Pappas is working on replacements for ``fslex`` and ``fsyacc`` (see th `facio`_ repository).

.. _`facio: https://github.com/jack-pappas/facio
