\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}

-- | This is end-user code, which makes use of the 'UnitsOfMeasure'
-- library to do calculations.
module Examples where

import GHC.Prim (Proxy#, proxy#)
import UnitsOfMeasure
import Equations


-- Define our measure types:
-- hide the string behind an abstraction boundary?
type Kg = Base "kg"
type M  = Base "m"
type S  = Base "s"

-- Yuk.
gravityOnEarth = 9.808 `unit` (proxy# :: Proxy# (M /: (S *: S)))

-- :t gravityOnEarth
-- gravityOnEarth :: Quantity Double (M /: (S *: S))

heightOfBuilding = 40.0 `unit` (proxy# :: Proxy# M)

speedOfImpact = sqrt' (cast lemma (2.0 *: (gravityOnEarth *: heightOfBuilding)))
  where
    lemma :: ('One *: ((M /: (S *: S)) *: M)) :==: ((M /: S) *: (M /: S))
    lemma = commutative `trans` identity
            `trans` trustMe (proxy# :: Proxy# ((M /: (S *: S)) *: M))
                            (proxy# :: Proxy# ((M /: S) *: (M /: S)))

-- speedOfImpact' = sqrt (2.0 *: gravityOnEarth +: heightOfBuilding)
-- Couldn't match type ‘'One *: (M /: (S *: S))’ with ‘M’


myMass = 65.0 `unit` (proxy# :: Proxy# Kg)

type N = Kg *: (M /: (S *: S))

forceOnGround :: Quantity Double N
forceOnGround = myMass *: gravityOnEarth
\end{code}
