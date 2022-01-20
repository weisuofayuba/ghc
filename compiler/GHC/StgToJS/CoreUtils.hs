-- | Core utils
module GHC.StgToJS.CoreUtils where

import GHC.Prelude

import GHC.StgToJS.Types

import GHC.Stg.Syntax

import GHC.Tc.Utils.TcType

import GHC.Builtin.Types
import GHC.Builtin.Types.Prim

import GHC.Core.DataCon
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Type

import GHC.Types.RepType
import GHC.Types.Var
import GHC.Types.Id

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

-- | can we unbox C x to x, only if x is represented as a Number
isUnboxableCon :: DataCon -> Bool
isUnboxableCon dc
  | [t] <- dataConRepArgTys dc
  , [t1] <- typeVt (scaledThing t)
  = isUnboxable t1 &&
    dataConTag dc == 1 &&
    length (tyConDataCons $ dataConTyCon dc) == 1
  | otherwise = False

-- | one-constructor types with one primitive field represented as a JS Number
-- can be unboxed
isUnboxable :: VarType -> Bool
isUnboxable DoubleV = True
isUnboxable IntV    = True -- includes Char#
isUnboxable _       = False

varSize :: VarType -> Int
varSize VoidV = 0
varSize LongV = 2 -- hi, low
varSize AddrV = 2 -- obj/array, offset
varSize _     = 1

typeSize :: Type -> Int
typeSize t = sum . map varSize . typeVt $ t

isVoid :: VarType -> Bool
isVoid VoidV = True
isVoid _     = False

isPtr :: VarType -> Bool
isPtr PtrV = True
isPtr _    = False

isSingleVar :: VarType -> Bool
isSingleVar v = varSize v == 1

isMultiVar :: VarType -> Bool
isMultiVar v = varSize v > 1

-- | can we pattern match on these values in a case?
isMatchable :: [VarType] -> Bool
isMatchable [DoubleV] = True
isMatchable [IntV]    = True
isMatchable _         = False

tyConVt :: HasDebugCallStack => TyCon -> [VarType]
tyConVt = typeVt . mkTyConTy

idVt :: HasDebugCallStack => Id -> [VarType]
idVt = typeVt . idType

typeVt :: HasDebugCallStack => Type -> [VarType]
typeVt t | isRuntimeRepKindedTy t {- || isRuntimeRepTy t -} = []
typeVt t = map primRepVt (typePrimRep t)-- map uTypeVt (repTypeArgs t)

-- only use if you know it's not an unboxed tuple
uTypeVt :: HasDebugCallStack => UnaryType -> VarType
uTypeVt ut
  | isRuntimeRepKindedTy ut = VoidV
--  | isRuntimeRepTy ut = VoidV
  -- GHC panics on this otherwise
  | Just (tc, ty_args) <- splitTyConApp_maybe ut
  , length ty_args /= tyConArity tc = PtrV
  | isPrimitiveType ut = (primTypeVt ut)
  | otherwise          =
    case typePrimRep' ut of
      []   -> VoidV
      [pt] -> primRepVt pt
      _    -> pprPanic "uTypeVt: not unary" (ppr ut)

primRepVt :: HasDebugCallStack => PrimRep -> VarType
primRepVt VoidRep     = VoidV
primRepVt LiftedRep   = PtrV -- fixme does ByteArray# ever map to this?
primRepVt UnliftedRep = RtsObjV
primRepVt IntRep      = IntV
primRepVt Int8Rep     = IntV
primRepVt Int16Rep    = IntV
primRepVt Int32Rep    = IntV
primRepVt WordRep     = IntV
primRepVt Word8Rep    = IntV
primRepVt Word16Rep   = IntV
primRepVt Word32Rep   = IntV
primRepVt Int64Rep    = LongV
primRepVt Word64Rep   = LongV
primRepVt AddrRep     = AddrV
primRepVt FloatRep    = DoubleV
primRepVt DoubleRep   = DoubleV
primRepVt (VecRep{})  = error "uTypeVt: vector types are unsupported"

typePrimRep' :: HasDebugCallStack => UnaryType -> [PrimRep]
typePrimRep' ty = kindPrimRep' empty (typeKind ty)

-- | Find the primitive representation of a 'TyCon'. Defined here to
-- avoid module loops. Call this only on unlifted tycons.
tyConPrimRep' :: HasDebugCallStack => TyCon -> [PrimRep]
tyConPrimRep' tc = kindPrimRep' empty res_kind
  where
    res_kind = tyConResKind tc

-- | Take a kind (of shape @TYPE rr@) and produce the 'PrimRep's
-- of values of types of this kind.
kindPrimRep' :: HasDebugCallStack => SDoc -> Kind -> [PrimRep]
kindPrimRep' doc ki
  | Just ki' <- coreView ki
  = kindPrimRep' doc ki'
kindPrimRep' doc (TyConApp _typ [runtime_rep])
  = -- ASSERT( typ `hasKey` tYPETyConKey )
    runtimeRepPrimRep doc runtime_rep
kindPrimRep' doc ki
  = pprPanic "kindPrimRep'" (ppr ki $$ doc)

primTypeVt :: HasDebugCallStack => Type -> VarType
primTypeVt t = case tyConAppTyCon_maybe (unwrapType t) of
  Nothing -> error "primTypeVt: not a TyCon"
  Just tc
    | tc == charPrimTyCon              -> IntV
    | tc == intPrimTyCon               -> IntV
    | tc == wordPrimTyCon              -> IntV
    | tc == floatPrimTyCon             -> DoubleV
    | tc == doublePrimTyCon            -> DoubleV
    | tc == int8PrimTyCon              -> IntV
    | tc == word8PrimTyCon             -> IntV
    | tc == int16PrimTyCon             -> IntV
    | tc == word16PrimTyCon            -> IntV
    | tc == int32PrimTyCon             -> IntV
    | tc == word32PrimTyCon            -> IntV
    | tc == int64PrimTyCon             -> LongV
    | tc == word64PrimTyCon            -> LongV
    | tc == addrPrimTyCon              -> AddrV
    | tc == stablePtrPrimTyCon         -> AddrV
    | tc == stableNamePrimTyCon        -> RtsObjV
    | tc == statePrimTyCon             -> VoidV
    | tc == proxyPrimTyCon             -> VoidV
    | tc == realWorldTyCon             -> VoidV
    | tc == threadIdPrimTyCon          -> RtsObjV
    | tc == weakPrimTyCon              -> RtsObjV
    | tc == arrayPrimTyCon             -> ArrV
    | tc == smallArrayPrimTyCon        -> ArrV
    | tc == byteArrayPrimTyCon         -> ObjV -- can contain any JS reference, used for JSVal
    | tc == mutableArrayPrimTyCon      -> ArrV
    | tc == smallMutableArrayPrimTyCon -> ArrV
    | tc == mutableByteArrayPrimTyCon  -> ObjV -- can contain any JS reference, used for JSVal
    | tc == mutVarPrimTyCon            -> RtsObjV
    | tc == mVarPrimTyCon              -> RtsObjV
    | tc == tVarPrimTyCon              -> RtsObjV
    | tc == bcoPrimTyCon               -> RtsObjV -- fixme what do we need here?
    | tc == anyTyCon                   -> PtrV
    | tc == compactPrimTyCon           -> ObjV -- unsupported?
    | tc == eqPrimTyCon                -> VoidV -- coercion token?
    | tc == eqReprPrimTyCon            -> VoidV -- role
    | tc == unboxedUnitTyCon           -> VoidV -- Void#
    | otherwise                        -> pprPanic "primTypeVt: unrecognized primitive type" (ppr tc)

argVt :: StgArg -> VarType
argVt a = uTypeVt . stgArgType $ a

dataConType :: DataCon -> Type
dataConType dc = idType (dataConWrapId dc)

isBoolDataCon :: DataCon -> Bool
isBoolDataCon dc = isBoolTy (dataConType dc)

-- standard fixed layout: payload types
-- payload starts at .d1 for heap objects, entry closest to Sp for stack frames
fixedLayout :: [VarType] -> CILayout
fixedLayout vts = CILayoutFixed (sum (map varSize vts)) vts

-- 2-var values might have been moved around separately, use DoubleV as substitute
-- ObjV is 1 var, so this is no problem for implicit metadata
stackSlotType :: Id -> VarType
stackSlotType i
  | varSize otype == 1 = otype
  | otherwise          = DoubleV
  where otype = uTypeVt (idType i)

idTarget :: Id -> [(PrimRep, Int)]
idTarget = typeTarget . idType

typeTarget :: Type -> [(PrimRep, Int)]
typeTarget = map (\t -> (t, varSize (primRepVt t))) . typePrimRep . unwrapType

alignTarget :: [(PrimRep, Int)] -> [a] -> [(PrimRep, [a])]
alignTarget []     _  = []
alignTarget ((rep, size):xs) vs
  | length vs0 == size = (rep, vs0) : alignTarget xs vs1
  | otherwise          = panic "alignTarget: target size insufficient"
  where (vs0, vs1) = splitAt size vs

