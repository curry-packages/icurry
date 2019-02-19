ICurry
======

ICurry is an intermediate format to compile Curry to different imperative
languages.
Its purpose is to be mostly generic, so any target language can be supported
with a similar effort.

The definition of ICurry is inspired by the Curry compiler
[Sprite](http://dx.doi.org/10.1007/978-3-319-63139-4_6)
which compiles Curry programs into LLVM code.

The `icurry` package includes a format `ICurry` (see `ICurry.Types`),
a format `Extended ICurry` (see `ICurry.Extended.Types`), a translator
from Flat- to ICurry, from I- to Extended ICurry, some goodies for
dealing with (Extended) ICurry structures and parts to quickly create a build
system based on Ninja.
The translators are available in the `icurry` binary installed
with this package.

