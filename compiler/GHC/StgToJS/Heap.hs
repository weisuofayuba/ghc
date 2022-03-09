{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module GHC.StgToJS.Heap
  ( closureType
  , entryClosureType
  , isObject
  , isThunk
  , isThunk'
  , isBlackhole
  , isFun
  , isFun'
  , isPap
  , isPap'
  , isCon
  , isCon'
  , conTag
  , conTag'
  , entry
  , funArity
  , funArity'
  , papArity
  , funOrPapArity
  -- * Field names
  , closureEntry_
  , closureMeta_
  , closureExtra1_
  , closureExtra2_
  )
where

import GHC.Prelude

import GHC.JS.Syntax
import GHC.JS.Make
import GHC.StgToJS.Types
import GHC.Data.ShortText (ShortText)

-- Note [JS heap objects]
-- ~~~~~~~~~~~~~~~~~~~~~~
--
-- Objects on the heap ("closures") are represented as JavaScript objects with
-- the following fields:
--
--  { f: function -- entry function
--  , m: meta     -- meta data
--  , d1: x       -- closure specific fields
--  , d2: y
--  }
--
-- Every heap object has an entry function "f".
--
-- Similarly to info tables in native code generation, the JS function object
-- "f" also contains some metadata about the Haskell object:
--
--    { t: closure type
--    , a: constructor tag / fun arity
--    }
--
-- Note that functions in JS are objects so if "f" is a function we can:
--  - call it, e.g. "f(arg0,arg1...)"
--  - get/set its metadata, e.g. "var closureType = f.t"
--
-- THUNK =
--  { f  = returns the object reduced to WHNF
--  , m  = ?
--  , d1 = ?
--  , d2 = ?
--  }
--
-- FUN =
--  { f  = function itself
--  , m  = ?
--  , d1 = free variable 1
--  , d2 = free variable 2
--  }
--
-- PAP =
--  { f  = ?
--  , m  = ?
--  , d1 = ?
--  , d2 =
--    { d1 = PAP arity
--    }
--  }
--
-- CON =
--  { f  = entry function of the datacon worker
--  , m  = 0
--  , d1 = first arg
--  , d2 = arity = 2: second arg
--         arity > 2: { d1, d2, ...} object with remaining args (starts with "d1 = x2"!)
--  }
--
-- BLACKHOLE =
--  { f  = h$blackhole
--  , m  = ?
--  , d1 = owning TSO
--  , d2 = waiters array
--  }
--
-- StackFrame closures are *not* represented as JS objects. Instead they are
-- "unpacked" in the stack, i.e. a stack frame occupies a few slots in the JS
-- array representing the stack ("h$stack").

closureEntry_ :: ShortText
closureEntry_ = "f"

closureExtra1_ :: ShortText
closureExtra1_ = "d1"

closureExtra2_ :: ShortText
closureExtra2_ = "d2"

closureMeta_ :: ShortText
closureMeta_ = "m"

entryClosureType_ :: ShortText
entryClosureType_ = "t"

entryConTag_ :: ShortText
entryConTag_ = "a"

entryFunArity_ :: ShortText
entryFunArity_ = "a"



closureType :: JExpr -> JExpr
closureType = entryClosureType . entry

entryClosureType :: JExpr -> JExpr
entryClosureType f = f .^ entryClosureType_

isObject :: JExpr -> JExpr
isObject c = typeof c .===. String "object"

isThunk :: JExpr -> JExpr
isThunk c = closureType c .===. toJExpr Thunk

isThunk' :: JExpr -> JExpr
isThunk' f = entryClosureType f .===. toJExpr Thunk

isBlackhole :: JExpr -> JExpr
isBlackhole c = closureType c .===. toJExpr Blackhole

isFun :: JExpr -> JExpr
isFun c = closureType c .===. toJExpr Fun

isFun' :: JExpr -> JExpr
isFun' f = entryClosureType f .===. toJExpr Fun

isPap :: JExpr -> JExpr
isPap c = closureType c .===. toJExpr Pap

isPap' :: JExpr -> JExpr
isPap' f = entryClosureType f .===. toJExpr Pap

isCon :: JExpr -> JExpr
isCon c = closureType c .===. toJExpr Con

isCon' :: JExpr -> JExpr
isCon' f = entryClosureType f .===. toJExpr Con

conTag :: JExpr -> JExpr
conTag = conTag' . entry

conTag' :: JExpr -> JExpr
conTag' f = f .^ entryConTag_

entry :: JExpr -> JExpr
entry p = p .^ closureEntry_

-- number of  arguments (arity & 0xff = arguments, arity >> 8 = number of registers)
funArity :: JExpr -> JExpr
funArity = funArity' . entry

-- function arity with raw reference to the entry
funArity' :: JExpr -> JExpr
funArity' f = f .^ entryFunArity_

-- arity of a partial application
papArity :: JExpr -> JExpr
papArity cp = cp .^ closureExtra2_ .^ closureExtra1_

funOrPapArity
  :: JExpr       -- ^ heap object
  -> Maybe JExpr -- ^ reference to entry, if you have one already (saves a c.f lookup twice)
  -> JExpr       -- ^ arity tag (tag >> 8 = registers, tag & 0xff = arguments)
funOrPapArity c = \case
  Nothing -> ((IfExpr (toJExpr (isFun c))) (toJExpr (funArity c)))
             (toJExpr (papArity c))
  Just f  -> ((IfExpr (toJExpr (isFun' f))) (toJExpr (funArity' f)))
             (toJExpr (papArity c))
