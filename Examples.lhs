\begin{code}
module Examples where

import UnitsOfMeasure
import GHC.Prim (Proxy#, proxy#)


-- Define our measure types:
-- hide the string behind an abstraction boundary?
type Kg = Base "kg"
type M  = Base "m"
type S  = Base "s"

-- Yuk.
gravityOnEarth = 9.808 `unit` (proxy# :: Proxy# (M :/: (S :^: 2)))

-- :t gravityOnEarth
-- gravityOnEarth :: Quantity Double (M :/: (S :^: 2))

heightOfBuilding = 40.0 `unit` (proxy# :: Proxy# M)

speedOfImpact = sqrt' (cast lemma (2.0 *: (gravityOnEarth *: heightOfBuilding)))
  where
    lemma :: ('One :*: ((M :/: (S :^: 2)) :*: M)) :==: ((M :/: S) ':*: (M :/: S))
    lemma = trustMe (proxy# :: Proxy# (One :*: ((M :/: (S :^: 2)) :*: M))) (proxy# :: Proxy# ((M :/: S) :*: (M :/: S)))

-- speedOfImpact' = sqrt (2.0 *: gravityOnEarth +: heightOfBuilding)
-- Couldn't match type ‘'One :*: (M :/: (S :^: 2))’ with ‘M’


myMass = 65.0 `unit` (proxy# :: Proxy# Kg)

type N = Kg :*: (M :/: S :^: 2)

forceOnGround :: Quantity Double N
forceOnGround = myMass *: gravityOnEarth
\end{code}
