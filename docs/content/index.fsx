(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
fsharp-tapl
======================

Code and Examples from Benjamin Pierce's ["Types and Programming Languages"](http://www.cis.upenn.edu/~bcpierce/tapl/), a comprehensive introduction to type systems and programming language theory. The code which accompanies the book is written in OCaml. This repository contains an F# port of that code, using FsLexYacc for lexing and parsing.

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The fsharp-tapl library can be <a href="https://nuget.org/packages/fsharp-tapl">installed from NuGet</a>:
      <pre>PM> Install-Package fsharp-tapl</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

Getting Started
-------

Run the build script to download dependencies, build all projects, and run regression tests.

All projects are console apps. Run with file input 

``
>cd bin\relase
> untyped -i ..\..\untyped\test.f
``
or input from command line
``
>arith -s "iszero (pred (succ 0));"
``

Samples & documentation
-----------------------

 * [TAPL to program reference](taplReference.html), sample tapl programs referenced by TAPL table of contents.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the libraries. This includes additional brief samples on using most of the
   functions.

 * [Implementation Notes](notes.html)
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests.

The TAPL code is copyrighted by Benjamin C. Pierce and available under license, which allows modification and 
redistribution for both commercial and non-commercial purposes provided the copyright notice is retained. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/fsharp-tapl/tree/master/docs/content
  [gh]: https://github.com/fsprojects/fsharp-tapl
  [issues]: https://github.com/fsprojects/fsharp-tapl/issues
  [readme]: https://github.com/fsprojects/fsharp-tapl/blob/master/README.md
  [license]: https://github.com/fsprojects/fsharp-tapl/blob/master/LICENSE.txt
*)
