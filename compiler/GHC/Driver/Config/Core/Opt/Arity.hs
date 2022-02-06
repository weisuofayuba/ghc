module GHC.Driver.Config.Core.Opt.Arity
  ( initArityOpts
  ) where

import GHC.Prelude ()

import GHC.Driver.Session

import GHC.Core.Opt.Arity

initArityOpts :: DynFlags -> ArityOpts
initArityOpts dflags = ArityOpts
  { arity_pedBot     = gopt Opt_PedanticBottoms dflags
  , arity_dictsCheap = gopt Opt_DictsCheap dflags
  }
