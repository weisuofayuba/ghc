{-# LANGUAGE
             ScopedTypeVariables,
             TupleSections,
             OverloadedStrings
  #-}

{-
  The compactor does link-time optimization. It is much simpler
  than the Optimizer, no fancy dataflow analysis here.

  Optimizations:
  - rewrite all variables starting with h$$ to shorter names,
       these are internal names
  - write all function metadata compactly
 -}

module GHC.StgToJS.Linker.Compactor where
