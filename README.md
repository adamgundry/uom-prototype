Units of measure prototype
--------------------------

This is a prototype of units of measure for GHC Haskell, inspired by the F# implementation, and in particular [Types for Units-of-Measure: Theory and Practice](http://research.microsoft.com/en-us/um/people/akenn/units/CEFP09TypesForUnitsOfMeasure.pdf) by Andrew Kennedy.  It consists of:

 * `GHCUnits.lhs`: definition of a new kind `Unit` together with its operations; these will be wired-in to the compiler so that it can automatically solve constraints involving units.

 * `UnitsOfMeasure.lhs`: a library (defined outside the compiler) that exposes an interface for working with numeric quantities in a unit-safe way.

 * `Examples.lhs`: brief examples of using the library.

These three modules correspond to code that would reside in the compiler (base library), a separate units-of-measure library and an application using the library.  The advantage of this division is that the particular library interface for working with units (and how they relate to existing Haskell numeric types) need not be hard-wired into the compiler.

The next stage of the project will be to wire-in the `GHCUnits` definitions to the compiler, and extend the constraint solver to do abelian group unification.  We will also need to consider syntactic sugar for writing and presenting units.
