{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-|
    Types for Sire syntax trees (`Cmd`, `Exp`, etc).  "Sire.Syntax"
    parses `Rex` into concrete syntax trees, and "Sire.ReplExe"
    does... everything else (TODO)
-}
module Sire.Types
    ( Symb
    , Cmd(..)
    , Fun(..)
    , Exp(..)
    , XCmd
    , XExp
    , XFun
    , Fan
    , Defn(..)
    , Req(..)
    )
where

import PlunderPrelude

import Loot.Types (Symb)
import Plun       (Fan, LawName)
import Rex        (Rex)

---------------
-- Functions --
---------------

type XFun = Fun Fan Symb Symb
type XExp = Exp Fan Symb Symb
type XCmd = Cmd Fan Symb Symb

{-|
    A Sire function has an identifier for self-reference, a `LawName,
    a non-empty list of arguments, and an body expression.

    Note that when a function is bound (say with
    @(`ELET` v (`ELAM` (`FUN` w _ _ _)) _)@), there are two binders
    for the same function (@v@ and @w@).  @v@ is the binder used in the
    outside scope, and @w@ is use for self-references.  The difference
    doesn't matter during parsing, but it matters in code transformations.
-}
data Fun z v a = FUN v LawName (NonEmpty v) (Exp z v a)
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

{-|
    Sire Expressions. @z@ is the type of raw embedded Plunder values
    (created through macro-expansion), @v@ is the type of local variables,
    and @a@ is the type of free variables.

    The parser just treats all references as free variables.  Later on,
    name resolution splits them apart.
-}
data Exp z v a
    = EBED z                          -- ^ An embedded plunder value
    | EREF a                          -- ^ A free variable.
    | EVAR v                          -- ^ A bound variable.
    | ENAT Nat                        -- ^ A natural-number literal.
    | EAPP (Exp z v a) (Exp z v a)    -- ^ Function application
    | ELET v (Exp z v a) (Exp z v a)  -- ^ Let-binding
    | EREC v (Exp z v a) (Exp z v a)  -- ^ Self-recursive let binding.
    | ELAM (Fun z v a)                -- ^ Nested Function (Closure)
    | ELIN (NonEmpty (Exp z v a))     -- ^ Explicit Inline Application
    | EBAM (NonEmpty (Exp z v a))     -- ^ Explicit Compile-Time Application

    -- TODO Implement these using macros and replace.
    | ETAB (Map Nat (Exp z v a))      -- ^ Table literal
    | ECAB (Set Nat)                  -- ^ Table constructor
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

{-
    REQ_BLOCK  :: Set Nat -> IO (Nat, Val, Val)
    REQ_SUBMIT :: Nat -> Any -> IO ()
    REQ_CANCEL :: Nat -> IO ()
    REQ_FETCH  :: IO (Map Nat (Val, Maybe Val))
-}
data Req a
    = REQ_FETCH Symb
    | REQ_CANCEL Symb
    | REQ_SUBMIT (Symb, a)
    | REQ_BLOCK (Symb,Symb,Symb) (Set Symb)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

-------------------
-- REPL Commands --
-------------------

-- |Sire input commands.
data Cmd z v a
    = IMPORT [(Text, Maybe (Set Symb))]
    | FILTER [Symb]
    | OUTPUT (Exp z v a)          -- ^ @(e)@ Eval+print @e@
    | DUMPY (Exp z v a)           -- ^ @(<e)@ Eval+print @e@ and it's environment.
    | CHECK [([Rex], Exp z v a)]  -- ^ @??e@ Assert that e==1
    | DEFINE [Defn z v a]         -- ^ @(x=y)@, @((f x)=x)@ Define value,function.
    | SAVEV (Exp z v a)           -- ^ @(<<expr)@ Write an expression to disk.
    | XPLODE XExp                 -- ^ @(#?e)@ Trace macro expansions in @e@.
    | EFFECT (Req (Exp z v a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)

type Doc = Maybe Text

-- |A binder.  It's either a function (takes arguments) or a value
-- (does not).
data Defn z v a
    = BIND_FUN a Doc (Fun z v a)
    | BIND_EXP a Doc (Exp z v a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, NFData)
