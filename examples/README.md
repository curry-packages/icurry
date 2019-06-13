Compiling example programs
==========================

As mentioned in the README of the package, the current version
of the `icurry` compiler is intended to be invoked by
other build tools. Hence, the direct usage needs
some manual efforts. Thus, we describe below as
an example the direct translation of the example program `Rev.curry`
into the ICurry format.

Assumption:

* $CURRYHOME is defined as the main directory of the Curry implementation
  (KiCS2 or PAKCS)

1. Generate the typed FlatCurry files of module `Rev`
   (in `.curry/Rev.tfcy`) and the prelude (if PAKCS is used,
   replace `kics2` by `pakcs`):

       > $CURRYHOME/bin/kics2-frontend --extended -i $CURRYHOME/lib --typed-flat Rev
   
2. Generate `Prelude.icy` (the ICurry representation of the Prelude,
   which also generates the ICurry type dependency file `Prelude.ictdeps`):

       > icurry f2i -I $CURRYHOME/lib/.curry $CURRYHOME/lib/.curry/Prelude.tfcy $CURRYHOME/lib/.curry/Prelude.icy

   Note that this takes a *lot of time* if `icurry` has been generated
   with PAKCS instead of KiCS2!

3. Now one can compile modules using the prelude in the same way.
   For instance, to compile module `Rev` in this directory:

       > icurry f2i -I .curry -I $CURRYHOME/lib/.curry .curry/Rev.tfcy .curry/Rev.icy

   With the additional option `-p`, the a pretty-printed version of the
   generated ICurry program is shown on the terminal:

       > icurry f2i -p -I .curry -I $CURRYHOME/lib/.curry .curry/Rev.tfcy .curry/Rev.icy

4. If desired, generate the Extended ICurry representation of `Rev`
   (in `.curry/Rev.eicy`):

       > icurry i2e -I .curry -I $CURRYHOME/lib/.curry .curry/Rev.icy .curry/Rev.eicy

   or similarly for the prelude:

       > icurry i2e -I $CURRYHOME/lib/.curry $CURRYHOME/lib/.curry/Prelude.icy $CURRYHOME/lib/.curry/Prelude.eicy

----------------------------------------------------------------------------
