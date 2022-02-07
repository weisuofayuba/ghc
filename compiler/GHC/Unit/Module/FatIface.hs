module GHC.Unit.Module.FatIface where

import GHC.Prelude
import GHC.Unit.Types (Module)
import GHC.Unit.Module.Location
import GHC.Iface.Syntax
import GHC.Utils.Binary

data FatIface = FatIface { fi_bindings :: [IfaceBinding IfaceTopBndrInfo ]
                         , fi_module   :: Module
                         , fi_mod_location :: ModLocation
                         }


instance Binary FatIface where
  put_ bh (FatIface a b c) = put_ bh a >> put_ bh b >> put_ bh c
  get bh = FatIface <$> get bh <*> get bh <*> get bh
