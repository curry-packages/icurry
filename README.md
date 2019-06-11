icurry
======

ICurry is an intermediate format to compile Curry to different imperative
languages.
Its purpose is to be mostly generic so that different target languages
can be supported with a similar effort.

The definition of ICurry is inspired by the Curry compiler
[Sprite](http://dx.doi.org/10.1007/978-3-319-63139-4_6)
which compiles Curry programs into LLVM code.

The `icurry` package supports two kinds of intermediate formats:

* `ICurry` (see `ICurry.Types` for detailed definitions)
* `Extended ICurry` (see `ICurry.Extended.Types` for detailed definitions)

The package also contains a translator from FlatCurry to ICurry programs
(which have the suffix `.icy`), a translator from ICurry to
Extended ICurry programs (which have the suffix `.eicy`), some goodies for
dealing with (Extended) ICurry structures and auxiliary to quickly
create a build system based on Ninja (see package `ninja`).

The translators are available in the `icurry` binary installed
with this package.

Usage:
------

In contrast to other general compilers which can translate
complete applications consisting of several modules,
the `icurry` compiler translates only single modules
so that it should be used as a tool invoked by
appropriate build tools (e.g., `ninja`, `make`).
In particular, before compiling some module, all its
imported modules have to be compiled so that their interfaces
(data and function types) are already stored in
ICurry type dependencies files (suffix `.ictdeps`).

For instance, to compile the Prelude manually,
one can change to the library directory containing
the file `Prelude.curry` and invoke the following commands:

1. Generate `Prelude.tfcy` (the typed FlatCurry file of the Prelude):

       > pakcs-frontend -i "." --typed-flat Prelude
   
   or
    
       > kics2-frontend -i "." --typed-flat Prelude

2. Generate `Prelude.icy` (the ICurry representation of the Prelude,
   which also generates the ICurry type dependency file `Prelude.ictdeps`):

       > icurry f2i -I ".curry" .curry/Prelude.tfcy .curry/Prelude.icy

3. Generate `Prelude.eicy` (the Extended ICurry representation of the Prelude):

       > icurry i2e -I ".curry" .curry/Prelude.icy .curry/Prelude.eicy

Now one can compile modules using the Prelude in the same way.

----------------------------------------------------------------------------
