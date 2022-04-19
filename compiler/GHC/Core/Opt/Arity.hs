{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


        Arity and eta expansion
-}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Arity and eta expansion
module GHC.Core.Opt.Arity
   ( -- Finding arity
     manifestArity, joinRhsArity, exprArity
   , findRhsArity, exprBotStrictness_maybe

   -- ** Eta expansion
   , exprEtaExpandArity, etaExpand, etaExpandAT

   -- ** Eta reduction
   , tryEtaReduce

   -- ** ArityType
   , ArityType, mkBotArityType, mkManifestArityType
   , arityTypeArity, idArityType, getBotArity

   -- ** typeArity and the state hack
   , typeArity, typeOneShots, typeOneShot
   , isOneShotBndr
   , isStateHackType

   -- * Lambdas
   , zapLamBndrs


   -- ** Join points
   , etaExpandToJoinPoint, etaExpandToJoinPointRule

   -- ** Coercions and casts
   , pushCoArg, pushCoArgs, pushCoValArg, pushCoTyArg
   , pushCoercionIntoLambda, pushCoDataCon, collectBindersPushingCo
   )
where

import GHC.Prelude

import GHC.Driver.Session ( DynFlags, GeneralFlag(..), gopt )

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Utils
import GHC.Core.DataCon
import GHC.Core.TyCon     ( tyConArity )
import GHC.Core.TyCon.RecWalk     ( initRecTc, checkRecTc )
import GHC.Core.Predicate ( isDictTy, isEvVar, isCallStackPredTy )
import GHC.Core.Multiplicity

-- We have two sorts of substitution:
--   GHC.Core.Subst.Subst, and GHC.Core.TyCo.TCvSubst
-- Both have substTy, substCo  Hence need for qualification
import GHC.Core.Subst    as Core hiding ( extendInScopeSet )
import GHC.Core.Type     as Type
import GHC.Core.Coercion as Type

import GHC.Types.Demand
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Types.Tickish

import GHC.Builtin.Types.Prim
import GHC.Builtin.Uniques

import GHC.Data.FastString
import GHC.Data.Pair

import GHC.Utils.GlobalVars( unsafeHasNoStateHack )
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Trace
import GHC.Utils.Misc

{-
************************************************************************
*                                                                      *
              manifestArity and exprArity
*                                                                      *
************************************************************************

exprArity is a cheap-and-cheerful version of exprEtaExpandArity.
It tells how many things the expression can be applied to before doing
any work.  It doesn't look inside cases, lets, etc.  The idea is that
exprEtaExpandArity will do the hard work, leaving something that's easy
for exprArity to grapple with.  In particular, Simplify uses exprArity to
compute the ArityInfo for the Id.

Originally I thought that it was enough just to look for top-level lambdas, but
it isn't.  I've seen this

        foo = PrelBase.timesInt

We want foo to get arity 2 even though the eta-expander will leave it
unchanged, in the expectation that it'll be inlined.  But occasionally it
isn't, because foo is blacklisted (used in a rule).

Similarly, see the ok_note check in exprEtaExpandArity.  So
        f = __inline_me (\x -> e)
won't be eta-expanded.

And in any case it seems more robust to have exprArity be a bit more intelligent.
But note that   (\x y z -> f x y z)
should have arity 3, regardless of f's arity.
-}

manifestArity :: CoreExpr -> Arity
-- ^ manifestArity sees how many leading value lambdas there are,
--   after looking through casts
manifestArity (Lam v e) | isId v        = 1 + manifestArity e
                        | otherwise     = manifestArity e
manifestArity (Tick t e) | not (tickishIsCode t) =  manifestArity e
manifestArity (Cast e _)                = manifestArity e
manifestArity _                         = 0

joinRhsArity :: CoreExpr -> JoinArity
-- Join points are supposed to have manifestly-visible
-- lambdas at the top: no ticks, no casts, nothing
-- Moreover, type lambdas count in JoinArity
joinRhsArity (Lam _ e) = 1 + joinRhsArity e
joinRhsArity _         = 0


---------------
exprArity :: CoreExpr -> Arity
-- ^ An approximate, fast, version of 'exprEtaExpandArity'
-- We do /not/ guarantee that exprArity e <= typeArity e
-- You may need to do arity trimming after calling exprArity
-- See Note [Arity trimming]
-- Reason: if we do arity trimming here we have take exprType
--         and that can be expensive if there is a large cast
exprArity e = go e
  where
    go (Var v)                     = idArity v
    go (Lam x e) | isId x          = go e + 1
                 | otherwise       = go e
    go (Tick t e) | not (tickishIsCode t) = go e
    go (Cast e _)                  = go e
    go (App e (Type _))            = go e
    go (App f a) | exprIsTrivial a = (go f - 1) `max` 0
        -- See Note [exprArity for applications]
        -- NB: coercions count as a value argument

    go _                           = 0

---------------
exprBotStrictness_maybe :: CoreExpr -> Maybe (Arity, DmdSig)
-- A cheap and cheerful function that identifies bottoming functions
-- and gives them a suitable strictness signatures.  It's used during
-- float-out
exprBotStrictness_maybe e
  = case getBotArity (arityType botStrictnessArityEnv e) of
        Nothing -> Nothing
        Just ar -> Just (ar, mkVanillaDmdSig ar botDiv)

{- Note [exprArity for applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we come to an application we check that the arg is trivial.
   eg  f (fac x) does not have arity 2,
                 even if f has arity 3!

* We require that is trivial rather merely cheap.  Suppose f has arity 2.
  Then    f (Just y)
  has arity 0, because if we gave it arity 1 and then inlined f we'd get
          let v = Just y in \w. <f-body>
  which has arity 0.  And we try to maintain the invariant that we don't
  have arity decreases.

*  The `max 0` is important!  (\x y -> f x) has arity 2, even if f is
   unknown, hence arity 0


************************************************************************
*                                                                      *
              typeArity and the "state hack"
*                                                                      *
********************************************************************* -}


typeArity :: Type -> Arity
-- ^ (typeArity ty) says how many arrows GHC can expose in 'ty', after
-- looking through newtypes.  More generally, (typeOneShots ty) returns
-- ty's [OneShotInfo], based only on the type itself, using typeOneShot
-- on the argument type to access the "state hack".
typeArity = length . typeOneShots

typeOneShots :: Type -> [OneShotInfo]
-- How many value arrows are visible in the type?
-- We look through foralls, and newtypes
-- See Note [Arity invariants for bindings]
typeOneShots ty
  = go initRecTc ty
  where
    go rec_nts ty
      | Just (_, ty')  <- splitForAllTyCoVar_maybe ty
      = go rec_nts ty'

      | Just (_,arg,res) <- splitFunTy_maybe ty
      = typeOneShot arg : go rec_nts res

      | Just (tc,tys) <- splitTyConApp_maybe ty
      , Just (ty', _) <- instNewTyCon_maybe tc tys
      , Just rec_nts' <- checkRecTc rec_nts tc  -- See Note [Expanding newtypes and products]
                                                -- in GHC.Core.TyCon
--   , not (isClassTyCon tc)    -- Do not eta-expand through newtype classes
--                              -- See Note [Newtype classes and eta expansion]
--                              (no longer required)
      = go rec_nts' ty'
        -- Important to look through non-recursive newtypes, so that, eg
        --      (f x)   where f has arity 2, f :: Int -> IO ()
        -- Here we want to get arity 1 for the result!
        --
        -- AND through a layer of recursive newtypes
        -- e.g. newtype Stream m a b = Stream (m (Either b (a, Stream m a b)))

      | otherwise
      = []

typeOneShot :: Type -> OneShotInfo
typeOneShot ty
   | isStateHackType ty = OneShotLam
   | otherwise          = NoOneShotInfo

-- | Like 'idOneShotInfo', but taking the Horrible State Hack in to account
-- See Note [The state-transformer hack] in "GHC.Core.Opt.Arity"
idStateHackOneShotInfo :: Id -> OneShotInfo
idStateHackOneShotInfo id
    | isStateHackType (idType id) = OneShotLam
    | otherwise                   = idOneShotInfo id

-- | Returns whether the lambda associated with the 'Id' is
--   certainly applied at most once
-- This one is the "business end", called externally.
-- It works on type variables as well as Ids, returning True
-- Its main purpose is to encapsulate the Horrible State Hack
-- See Note [The state-transformer hack] in "GHC.Core.Opt.Arity"
isOneShotBndr :: Var -> Bool
isOneShotBndr var
  | isTyVar var                              = True
  | OneShotLam <- idStateHackOneShotInfo var = True
  | otherwise                                = False

isStateHackType :: Type -> Bool
isStateHackType ty
  | unsafeHasNoStateHack   -- Switch off with -fno-state-hack
  = False
  | otherwise
  = case tyConAppTyCon_maybe ty of
        Just tycon -> tycon == statePrimTyCon
        _          -> False
        -- This is a gross hack.  It claims that
        -- every function over realWorldStatePrimTy is a one-shot
        -- function.  This is pretty true in practice, and makes a big
        -- difference.  For example, consider
        --      a `thenST` \ r -> ...E...
        -- The early full laziness pass, if it doesn't know that r is one-shot
        -- will pull out E (let's say it doesn't mention r) to give
        --      let lvl = E in a `thenST` \ r -> ...lvl...
        -- When `thenST` gets inlined, we end up with
        --      let lvl = E in \s -> case a s of (r, s') -> ...lvl...
        -- and we don't re-inline E.
        --
        -- It would be better to spot that r was one-shot to start with, but
        -- I don't want to rely on that.
        --
        -- Another good example is in fill_in in PrelPack.hs.  We should be able to
        -- spot that fill_in has arity 2 (and when Keith is done, we will) but we can't yet.


{- Note [Arity invariants for bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have the following invariants for let-bindings

  (1) In any binding f = e,
         idArity f <= typeArity (idType f)
      We enforce this with trimArityType, called in findRhsArity;
      see Note [Arity trimming].

      Note that we enforce this only for /bindings/.  We do /not/ insist that
         arityTypeArity (arityType e) <= typeArity (exprType e)
      because that is quite a bit more expensive to guaranteed; it would
      mean checking at every Cast in the recursive arityType, for example.

  (2) If typeArity (exprType e) = n,
      then manifestArity (etaExpand e n) = n

      That is, etaExpand can always expand as much as typeArity says
      (or less, of course). So the case analysis in etaExpand and in
      typeArity must match.

      Consequence: because of (1), if we eta-expand to (idArity f), we will
      end up with n manifest lambdas.

   (3) In any binding f = e,
         idArity f <= arityTypeArity (safeArityType (arityType e))
       That is, we call safeArityType before attributing e's arityType to f.
       See Note [SafeArityType].

       So we call safeArityType in findRhsArity.

Suppose we have
   f :: Int -> Int -> Int
   f x y = x+y    -- Arity 2

   g :: F Int
   g = case <cond> of { True  -> f |> co1
                      ; False -> g |> co2 }

where F is a type family.  Now, we can't eta-expand g to have arity 2,
because etaExpand, which works off the /type/ of the expression
(albeit looking through newtypes), doesn't know how to make an
eta-expanded binding
   g = (\a b. case x of ...) |> co
because it can't make up `co` or the types of `a` and `b`.

So invariant (1) ensures that every binding has an arity that is no greater
than the typeArity of the RHS; and invariant (2) ensures that etaExpand
and handle what typeArity says.

Why is this important?  Because

  - In GHC.Iface.Tidy we use exprArity/manifestArity to fix the *final
    arity* of each top-level Id, and in

  - In CorePrep we use etaExpand on each rhs, so that the visible
    lambdas actually match that arity, which in turn means that the
    StgRhs has a number of lambdas that precisely matches the arity.

Note [Arity trimming]
~~~~~~~~~~~~~~~~~~~~~
Invariant (1) of Note [Arity invariants for bindings] is upheld by findRhsArity,
which calls trimArityType to trim the ArityType to match the Arity of the
binding.  Failing to do so, and hence breaking invariant (1) led to #5441.

How to trim?  If we end in topDiv, it's easy.  But we must take great care with
dead ends (i.e. botDiv). Suppose the expression was (\x y. error "urk"),
we'll get \??.⊥.  We absolutely must not trim that to \?.⊥, because that
claims that ((\x y. error "urk") |> co) diverges when given one argument,
which it absolutely does not. And Bad Things happen if we think something
returns bottom when it doesn't (#16066).

So, if we need to trim a dead-ending arity type, switch (conservatively) to
topDiv.

Historical note: long ago, we unconditionally switched to topDiv when we
encountered a cast, but that is far too conservative: see #5475

Note [Newtype classes and eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    NB: this nasty special case is no longer required, because
    for newtype classes we don't use the class-op rule mechanism
    at all.  See Note [Single-method classes] in GHC.Tc.TyCl.Instance. SLPJ May 2013

-------- Old out of date comments, just for interest -----------
We have to be careful when eta-expanding through newtypes.  In general
it's a good idea, but annoyingly it interacts badly with the class-op
rule mechanism.  Consider

   class C a where { op :: a -> a }
   instance C b => C [b] where
     op x = ...

These translate to

   co :: forall a. (a->a) ~ C a

   $copList :: C b -> [b] -> [b]
   $copList d x = ...

   $dfList :: C b -> C [b]
   {-# DFunUnfolding = [$copList] #-}
   $dfList d = $copList d |> co@[b]

Now suppose we have:

   dCInt :: C Int

   blah :: [Int] -> [Int]
   blah = op ($dfList dCInt)

Now we want the built-in op/$dfList rule will fire to give
   blah = $copList dCInt

But with eta-expansion 'blah' might (and in #3772, which is
slightly more complicated, does) turn into

   blah = op (\eta. ($dfList dCInt |> sym co) eta)

and now it is *much* harder for the op/$dfList rule to fire, because
exprIsConApp_maybe won't hold of the argument to op.  I considered
trying to *make* it hold, but it's tricky and I gave up.

The test simplCore/should_compile/T3722 is an excellent example.
-------- End of old out of date comments, just for interest -----------
-}

{- ********************************************************************
*                                                                      *
                  Zapping lambda binders
*                                                                      *
********************************************************************* -}

zapLamBndrs :: FullArgCount -> [Var] -> [Var]
-- If (\xyz. t) appears under-applied to only two arguments,
-- we must zap the occ-info on x,y, because they appear (in 't') under the \z.
-- See Note [Occurrence analysis for lambda binders] in GHc.Core.Opt.OccurAnal
--
-- NB: both `arg_count` and `bndrs` include both type and value args/bndrs
zapLamBndrs arg_count bndrs
  | no_need_to_zap = bndrs
  | otherwise      = zap_em arg_count bndrs
  where
    no_need_to_zap = all isOneShotBndr (drop arg_count bndrs)

    zap_em :: FullArgCount -> [Var] -> [Var]
    zap_em 0 bs = bs
    zap_em _ [] = []
    zap_em n (b:bs) | isTyVar b = b              : zap_em (n-1) bs
                    | otherwise = zapLamIdInfo b : zap_em (n-1) bs


{- *********************************************************************
*                                                                      *
           Computing the "arity" of an expression
*                                                                      *
************************************************************************

Note [Definition of arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The "arity" of an expression 'e' is n if
   applying 'e' to *fewer* than n *value* arguments
   converges rapidly

Or, to put it another way

   there is no work lost in duplicating the partial
   application (e x1 .. x(n-1))

In the divergent case, no work is lost by duplicating because if the thing
is evaluated once, that's the end of the program.

Or, to put it another way, in any context C

   C[ (\x1 .. xn. e x1 .. xn) ]
         is as efficient as
   C[ e ]

It's all a bit more subtle than it looks:

Note [One-shot lambdas]
~~~~~~~~~~~~~~~~~~~~~~~
Consider one-shot lambdas
                let x = expensive in \y z -> E
We want this to have arity 1 if the \y-abstraction is a 1-shot lambda.

Note [Dealing with bottom]
~~~~~~~~~~~~~~~~~~~~~~~~~~
A Big Deal with computing arities is expressions like

   f = \x -> case x of
               True  -> \s -> e1
               False -> \s -> e2

This happens all the time when f :: Bool -> IO ()
In this case we do eta-expand, in order to get that \s to the
top, and give f arity 2.

This isn't really right in the presence of seq.  Consider
        (f bot) `seq` 1

This should diverge!  But if we eta-expand, it won't.  We ignore this
"problem" (unless -fpedantic-bottoms is on), because being scrupulous
would lose an important transformation for many programs. (See
#5587 for an example.)

Consider also
        f = \x -> error "foo"
Here, arity 1 is fine.  But if it is
        f = \x -> case x of
                        True  -> error "foo"
                        False -> \y -> x+y
then we want to get arity 2.  Technically, this isn't quite right, because
        (f True) `seq` 1
should diverge, but it'll converge if we eta-expand f.  Nevertheless, we
do so; it improves some programs significantly, and increasing convergence
isn't a bad thing.  Hence the ABot/ATop in ArityType.

So these two transformations aren't always the Right Thing, and we
have several tickets reporting unexpected behaviour resulting from
this transformation.  So we try to limit it as much as possible:

 (1) Do NOT move a lambda outside a known-bottom case expression
       case undefined of { (a,b) -> \y -> e }
     This showed up in #5557

 (2) Do NOT move a lambda outside a case unless
     (a) The scrutinee is ok-for-speculation, or
     (b) more liberally: the scrutinee is cheap (e.g. a variable), and
         -fpedantic-bottoms is not enforced (see #2915 for an example)

Of course both (1) and (2) are readily defeated by disguising the bottoms.

4. Note [Newtype arity]
~~~~~~~~~~~~~~~~~~~~~~~~
Non-recursive newtypes are transparent, and should not get in the way.
We do (currently) eta-expand recursive newtypes too.  So if we have, say

        newtype T = MkT ([T] -> Int)

Suppose we have
        e = coerce T f
where f has arity 1.  Then: etaExpandArity e = 1;
that is, etaExpandArity looks through the coerce.

When we eta-expand e to arity 1: eta_expand 1 e T
we want to get:                  coerce T (\x::[T] -> (coerce ([T]->Int) e) x)

  HOWEVER, note that if you use coerce bogusly you can ge
        coerce Int negate
  And since negate has arity 2, you might try to eta expand.  But you can't
  decompose Int to a function type.   Hence the final case in eta_expand.

Note [The state-transformer hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
        f = e
where e has arity n.  Then, if we know from the context that f has
a usage type like
        t1 -> ... -> tn -1-> t(n+1) -1-> ... -1-> tm -> ...
then we can expand the arity to m.  This usage type says that
any application (x e1 .. en) will be applied to uniquely to (m-n) more args
Consider f = \x. let y = <expensive>
                 in case x of
                      True  -> foo
                      False -> \(s:RealWorld) -> e
where foo has arity 1.  Then we want the state hack to
apply to foo too, so we can eta expand the case.

Then we expect that if f is applied to one arg, it'll be applied to two
(that's the hack -- we don't really know, and sometimes it's false)
See also Id.isOneShotBndr.

Note [State hack and bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's a terrible idea to use the state hack on a bottoming function.
Here's what happens (#2861):

  f :: String -> IO T
  f = \p. error "..."

Eta-expand, using the state hack:

  f = \p. (\s. ((error "...") |> g1) s) |> g2
  g1 :: IO T ~ (S -> (S,T))
  g2 :: (S -> (S,T)) ~ IO T

Extrude the g2

  f' = \p. \s. ((error "...") |> g1) s
  f = f' |> (String -> g2)

Discard args for bottomming function

  f' = \p. \s. ((error "...") |> g1 |> g3
  g3 :: (S -> (S,T)) ~ (S,T)

Extrude g1.g3

  f'' = \p. \s. (error "...")
  f' = f'' |> (String -> S -> g1.g3)

And now we can repeat the whole loop.  Aargh!  The bug is in applying the
state hack to a function which then swallows the argument.

This arose in another guise in #3959.  Here we had

     catch# (throw exn >> return ())

Note that (throw :: forall a e. Exn e => e -> a) is called with [a = IO ()].
After inlining (>>) we get

     catch# (\_. throw {IO ()} exn)

We must *not* eta-expand to

     catch# (\_ _. throw {...} exn)

because 'catch#' expects to get a (# _,_ #) after applying its argument to
a State#, not another function!

In short, we use the state hack to allow us to push let inside a lambda,
but not to introduce a new lambda.


Note [ArityType]
~~~~~~~~~~~~~~~~
ArityType can be thought of as an abstraction of an expression.
The ArityType
   AT [ (IsCheap,     NoOneShotInfo)
      , (IsExpensive, OneShotLam)
      , (IsCheap,     OneShotLam) ] Dunno)

abstracts an expression like
   \x. let <expensive> in
       \y{os}.
       \z{os}. blah

In general we have (AT lams div).  Then
* In lams :: [(Cost,OneShotInfo)]
  * The Cost flag describes the part of the expression down
    to the first (value) lambda.
  * The OneShotInfo flag gives the one-shot info on that lambda.

* If 'div' is dead-ending ('isDeadEndDiv'), then application to
  'length lams' arguments will surely diverge, similar to the situation
  with 'DmdType'.

ArityType is the result of a compositional analysis on expressions,
from which we can decide the real arity of the expression (extracted
with function exprEtaExpandArity).

We use the following notation:
  at  ::= \p1..pn.div
  div ::= T | x | ⊥
  p   ::= (c o)
  c   ::= X | C    -- Expensive or Cheap
  o   ::= ? | 1    -- NotOneShot or OneShotLam
We may omit the \. if n = 0.
And ⊥ stands for `AT [] botDiv`

Here is an example demonstrating the notation:
  \(C?)(X1)(C1).T
stands for
   AT [ (IsCheap,NoOneShotInfo)
      , (IsExpensive,OneShotLam)
      , (IsCheap,OneShotLam) ]
      topDiv

See the 'Outputable' instance for more information. It's pretty simple.

How can we use ArityType?  Example:
      f = \x\y. let v = <expensive> in
          \s(one-shot) \t(one-shot). blah
      'f' has arity type \(C?)(C?)(X1)(C1).T
      The one-shot-ness means we can, in effect, push that
      'let' inside the \st, and expand to arity 4

Suppose f = \xy. x+y
Then  f             :: \(C?)(C?).T
      f v           :: \(C?).T
      f <expensive> :: \(X?).T

Here is what the fields mean. If an arbitrary expression 'f' has
ArityType 'at', then

 * If @at = AT [o1,..,on] botDiv@ (notation: \o1..on.⊥), then @f x1..xn@
   definitely diverges. Partial applications to fewer than n args may *or
   may not* diverge.  Ditto exnDiv.

 * If `f` has ArityType `at` we can eta-expand `f` to have (aritTypeOneShots at)
   arguments without losing sharing. This function checks that the either
   there are no expensive expressions, or the lambdas are one-shots.

   NB 'f' is an arbitrary expression, eg @f = g e1 e2@.  This 'f' can have
   arity type @AT oss _@, with @length oss > 0@, only if e1 e2 are themselves
   cheap.

 * In both cases, @f@, @f x1@, ... @f x1 ... x(n-1)@ are definitely
   really functions, or bottom, but *not* casts from a data type, in
   at least one case branch.  (If it's a function in one case branch but
   an unsafe cast from a data type in another, the program is bogus.)
   So eta expansion is dynamically ok; see Note [State hack and
   bottoming functions], the part about catch#

Wrinkles

* Wrinkle [Bottoming functions]: see function 'arityLam'.
  We treat bottoming functions as one-shot, because there is no point
  in floating work outside the lambda, and it's fine to float it inside.

  For example, this is fine (see test stranal/sigs/BottomFromInnerLambda)
       let x = <expensive> in \y. error (g x y)
       ==> \y. let x = <expensive> in error (g x y)

  Idea: perhaps we could enforce this invariant with
     data Arity Type = TopAT [(Cost, OneShotInfo)] | DivAT [Cost]


Note [SafeArityType]
~~~~~~~~~~~~~~~~~~~~
The function safeArityType trims an ArityType to return a "safe" ArityType,
for which we use a type synonym SafeAritType.  It is "safe" in the sense
that (arityTypeArity at) really reflects the arity of the expression, whereas
a regular ArityType might have more lambdas in its [ATLamInfo] that the
(cost-free) arity of the expression.

For example
   \x.\y.let v = expensive in \z. blah
has
   arityType = AT [C?, C?, X?, C?] Top
But the expression actually has arity 2, not 4, because of the X.
So safeArityType will trim it to (AT [C?, C?] Top), whose [ATLamInfo]
now reflects the (cost-free) arity of the expression

Why do we ever need an "unsafe" ArityType, such as the example above?
Because its (cost-free) arity may increased by combineWithDemandOneShots
in findRhsArity. See Note [Combining arity type with demand info].

Thus the function `arityType` returns a regular "unsafe" ArityType, that
goes deeply into the lambdas (including under IsExpensive). But that is
very local; most ArityTypes are indeed "safe".  We use the type synonym
SafeArityType to indicate where we think the ArityType is safe.
-}

-- | The analysis lattice of arity analysis. It is isomorphic to
--
-- @
--    data ArityType'
--      = AEnd Divergence
--      | ALam OneShotInfo ArityType'
-- @
--
-- Which is easier to display the Hasse diagram for:
--
-- @
--  ALam OneShotLam at
--          |
--      AEnd topDiv
--          |
--  ALam NoOneShotInfo at
--          |
--      AEnd exnDiv
--          |
--      AEnd botDiv
-- @
--
-- where the @at@ fields of @ALam@ are inductively subject to the same order.
-- That is, @ALam os at1 < ALam os at2@ iff @at1 < at2@.
--
-- Why the strange Top element? See Note [Combining case branches].
--
-- We rely on this lattice structure for fixed-point iteration in
-- 'findRhsArity'. For the semantics of 'ArityType', see Note [ArityType].
data ArityType  -- See Note [ArityType]
  = AT ![ATLamInfo] !Divergence
    -- ^ `AT oss div` is an abstraction of the expression, which describes
    -- its lambdas, and how much work appears where.
    -- See Note [ArityType] for more information
    --
    -- If `div` is dead-ending ('isDeadEndDiv'), then application to
    -- `length os` arguments will surely diverge, similar to the situation
    -- with 'DmdType'.
  deriving Eq

type ATLamInfo = (Cost,OneShotInfo)
     -- ^ Info about one lambda in an ArityType
     -- See Note [ArityType]

type SafeArityType = ArityType -- See Note [SafeArityType]

data Cost = IsCheap | IsExpensive
          deriving( Eq )

allCosts :: (a -> Cost) -> [a] -> Cost
allCosts f xs = foldr (addCost . f) IsCheap xs

addCost :: Cost -> Cost -> Cost
addCost IsCheap IsCheap = IsCheap
addCost _       _       = IsExpensive

-- | This is the BNF of the generated output:
--
-- @
-- @
--
-- We format
-- @AT [o1,..,on] topDiv@ as @\o1..on.T@ and
-- @AT [o1,..,on] botDiv@ as @\o1..on.⊥@, respectively.
-- More concretely, @AT [NOI,OS,OS] topDiv@ is formatted as @\?11.T@.
-- If the one-shot info is empty, we omit the leading @\.@.
instance Outputable ArityType where
  ppr (AT oss div)
    | null oss  = pp_div div
    | otherwise = char '\\' <> hcat (map pp_os oss) <> dot <> pp_div div
    where
      pp_div Diverges = char '⊥'
      pp_div ExnOrDiv = char 'x'
      pp_div Dunno    = char 'T'
      pp_os (IsCheap,     OneShotLam)    = text "(C1)"
      pp_os (IsExpensive, OneShotLam)    = text "(X1)"
      pp_os (IsCheap,     NoOneShotInfo) = text "(C?)"
      pp_os (IsExpensive, NoOneShotInfo) = text "(X?)"

mkBotArityType :: [OneShotInfo] -> ArityType
mkBotArityType oss = AT [(IsCheap,os) | os <- oss] botDiv

botArityType :: ArityType
botArityType = mkBotArityType []

mkManifestArityType :: [OneShotInfo] -> ArityType
mkManifestArityType oss = AT [(IsCheap,os) | os <- oss] topDiv

topArityType :: ArityType
topArityType = AT [] topDiv

-- | The number of value args for the arity type
arityTypeArity :: SafeArityType -> Arity
arityTypeArity (AT lams _) = length lams

arityTypeOneShots :: SafeArityType -> [OneShotInfo]
-- Returns a list only as long as the arity should be
arityTypeOneShots (AT lams _) = map snd lams

safeArityType :: ArityType -> SafeArityType
-- ^ Assuming this ArityType is all we know, find the arity of
-- the function, and trim the argument info (and Divergenge)
-- to match that arity. See Note [SafeArityType]
safeArityType at@(AT lams _)
  = case go 0 IsCheap lams of
      Nothing -> at  -- No trimming needed
      Just ar -> AT (take ar lams) topDiv
 where
   go :: Arity -> Cost -> [(Cost,OneShotInfo)] -> Maybe Arity
   go _ _ [] = Nothing
   go ar ch1 ((ch2,os):lams)
      = case (ch1 `addCost` ch2, os) of
          (IsExpensive, NoOneShotInfo) -> Just ar
          (ch,          _)             -> go (ar+1) ch lams

infixl 2 `trimArityType`

trimArityType :: Arity -> ArityType -> ArityType
-- ^ Trim an arity type so that it has at most the given arity.
-- Any excess 'OneShotInfo's are truncated to 'topDiv', even if
-- they end in 'ABot'.  See Note [Arity trimming]
trimArityType max_arity at@(AT lams _)
  | lams `lengthAtMost` max_arity = at
  | otherwise                    = AT (take max_arity lams) topDiv

-- | The Arity returned is the number of value args the
-- expression can be applied to without doing much work
exprEtaExpandArity :: DynFlags -> CoreExpr -> Maybe SafeArityType
-- exprEtaExpandArity is used when eta expanding
--      e  ==>  \xy -> e x y
-- Nothing if the expression has arity 0
exprEtaExpandArity dflags e
  | AT [] _ <- arity_type
  = Nothing
  | otherwise
  = Just arity_type
  where
    arity_type = safeArityType (arityType (etaExpandArityEnv dflags) e)

getBotArity :: ArityType -> Maybe Arity
-- Arity of a divergent function
getBotArity (AT oss div)
  | isDeadEndDiv div = Just $ length oss
  | otherwise        = Nothing


{- *********************************************************************
*                                                                      *
                   findRhsArity
*                                                                      *
********************************************************************* -}

findRhsArity :: DynFlags -> RecFlag -> Id -> CoreExpr -> Arity -> SafeArityType
-- This implements the fixpoint loop for arity analysis
-- See Note [Arity analysis]
-- If findRhsArity e = (n, is_bot) then
--  (a) any application of e to <n arguments will not do much work,
--      so it is safe to expand e  ==>  (\x1..xn. e x1 .. xn)
--  (b) if is_bot=True, then e applied to n args is guaranteed bottom
--
-- Returns an ArityType that is guaranteed trimmed to typeArity of 'bndr'
--         See Note [Arity trimming]
findRhsArity dflags is_rec bndr rhs old_arity
  = case is_rec of
      Recursive    -> go 0 botArityType
      NonRecursive -> step init_env
  where
    init_env :: ArityEnv
    init_env = findRhsArityEnv dflags

    ty_arity     = typeArity (idType bndr)
    id_one_shots = idDemandOneShots bndr

    step :: ArityEnv -> SafeArityType
    step env = trimArityType ty_arity $
               safeArityType $ -- See Note [Arity invariants for bindings], item (3)
               arityType env rhs `combineWithDemandOneShots` id_one_shots
       -- trimArityType: see Note [Trim arity inside the loop]
       -- combineWithDemandOneShots: take account of the demand on the
       -- binder.  Perhaps it is always called with 2 args
       --   let f = \x. blah in (f 3 4, f 1 9)
       -- f's demand-info says how many args it is called with

    -- The fixpoint iteration (go), done for recursive bindings. We
    -- always do one step, but usually that produces a result equal
    -- to old_arity, and then we stop right away, because old_arity
    -- is assumed to be sound. In other words, arities should never
    -- decrease.  Result: the common case is that there is just one
    -- iteration
    go :: Int -> SafeArityType -> SafeArityType
    go !n cur_at@(AT lams div)
      | not (isDeadEndDiv div)           -- the "stop right away" case
      , length lams <= old_arity = cur_at -- from above
      | next_at == cur_at       = cur_at
      | otherwise               =
         -- Warn if more than 2 iterations. Why 2? See Note [Exciting arity]
         warnPprTrace (debugIsOn && n > 2)
            "Exciting arity"
            (nest 2 (ppr bndr <+> ppr cur_at <+> ppr next_at $$ ppr rhs)) $
            go (n+1) next_at
      where
        next_at = step (extendSigEnv init_env bndr cur_at)

infixl 2 `combineWithDemandOneShots`

combineWithDemandOneShots :: ArityType -> [OneShotInfo] -> ArityType
-- See Note [Combining arity type with demand info]
combineWithDemandOneShots (AT lams div) oss
  = AT (zip_lams lams oss) div
  where
    zip_lams :: [ATLamInfo] -> [OneShotInfo] -> [ATLamInfo]
    zip_lams lams []  = lams
    zip_lams []   oss = [(IsExpensive,os) | os <- oss]
    zip_lams ((ch,os1):lams) (os2:oss)
      = (ch, os1 `bestOneShot` os2) : zip_lams lams oss

idDemandOneShots :: Id -> [OneShotInfo]
idDemandOneShots bndr
  = call_arity_one_shots `zip_lams` dmd_one_shots
  where
    call_arity_one_shots :: [OneShotInfo]
    call_arity_one_shots
      | call_arity == 0 = []
      | otherwise       = NoOneShotInfo : replicate (call_arity-1) OneShotLam
    -- Call Arity analysis says the function is always called
    -- applied to this many arguments.  The first NoOneShotInfo is because
    -- if Call Arity says "always applied to 3 args" then the one-shot info
    -- we get is [NoOneShotInfo, OneShotLam, OneShotLam]
    call_arity = idCallArity bndr

    dmd_one_shots :: [OneShotInfo]
    -- If the demand info is Cx(C1(C1(.))) then we know that an
    -- application to one arg is also an application to three
    dmd_one_shots = argOneShots (idDemandInfo bndr)

    -- Take the *longer* list
    zip_lams (lam1:lams1) (lam2:lams2) = (lam1 `bestOneShot` lam2) : zip_lams lams1 lams2
    zip_lams []           lams2        = lams2
    zip_lams lams1        []           = lams1

{- Note [Arity analysis]
~~~~~~~~~~~~~~~~~~~~~~~~
The motivating example for arity analysis is this:

  f = \x. let g = f (x+1)
          in \y. ...g...

What arity does f have?  Really it should have arity 2, but a naive
look at the RHS won't see that.  You need a fixpoint analysis which
says it has arity "infinity" the first time round.

This example happens a lot; it first showed up in Andy Gill's thesis,
fifteen years ago!  It also shows up in the code for 'rnf' on lists
in #4138.

We do the necessary, quite simple fixed-point iteration in 'findRhsArity',
which assumes for a single binding 'ABot' on the first run and iterates
until it finds a stable arity type. Two wrinkles

* We often have to ask (see the Case or Let case of 'arityType') whether some
  expression is cheap. In the case of an application, that depends on the arity
  of the application head! That's why we have our own version of 'exprIsCheap',
  'myExprIsCheap', that will integrate the optimistic arity types we have on
  f and g into the cheapness check.

* Consider this (#18793)

    go = \ds. case ds of
           []     -> id
           (x:ys) -> let acc = go ys in
                     case blah of
                       True  -> acc
                       False -> \ x1 -> acc (negate x1)

  We must propagate go's optimistically large arity to @acc@, so that the
  tail call to @acc@ in the True branch has sufficient arity.  This is done
  by the 'am_sigs' field in 'FindRhsArity', and 'lookupSigEnv' in the Var case
  of 'arityType'.

Note [Exciting arity]
~~~~~~~~~~~~~~~~~~~~~
The fixed-point iteration in 'findRhsArity' stabilises very quickly in almost
all cases. To get notified of cases where we need an usual number of iterations,
we emit a warning in debug mode, so that we can investigate and make sure that
we really can't do better. It's a gross hack, but catches real bugs (#18870).

Now, which number is "unusual"? We pick n > 2. Here's a pretty common and
expected example that takes two iterations and would ruin the specificity
of the warning (from T18937):

  f :: [Int] -> Int -> Int
  f []     = id
  f (x:xs) = let y = sum [0..x]
             in \z -> f xs (y + z)

Fixed-point iteration starts with arity type ⊥ for f. After the first
iteration, we get arity type \??.T, e.g. arity 2, because we unconditionally
'floatIn' the let-binding (see its bottom case).  After the second iteration,
we get arity type \?.T, e.g. arity 1, because now we are no longer allowed
to floatIn the non-cheap let-binding.  Which is all perfectly benign, but
means we do two iterations (well, actually 3 'step's to detect we are stable)
and don't want to emit the warning.

Note [Trim arity inside the loop]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here's an example (from gadt/nbe.hs) which caused trouble.
  data Exp g t where
     Lam :: Ty a -> Exp (g,a) b -> Exp g (a->b)

  eval :: Exp g t -> g -> t
  eval (Lam _ e) g = \a -> eval e (g,a)

The danger is that we get arity 3 from analysing this; and the
next time arity 4, and so on for ever.  Solution: use trimArityType
on each iteration.

Note [Combining arity type with demand info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   let f = \x. let y = <expensive> in \p \q{os}. blah
   in ...(f a b)...(f c d)...

* From the RHS we get an ArityType like
    AT [ (True,?), (False,?), (True,OneShotLam) ] Dunno
  where "?" means NoOneShotInfo

* From the body, the demand analyser, or Call Arity, will tell us
  that the function is always applied to at least two arguments.

Combining these two pieces of info, we can get the final ArityType
    AT [ (True,?), (False,OneShotLam), (True,OneShotLam) ] Dunno
result: arity=3, which is better than we could do from either
source alone.

The "combining" part is done by combineWithDemandOneShots.  It
uses info from both Call Arity and demand analysis.
-}


{- *********************************************************************
*                                                                      *
                   arityType
*                                                                      *
********************************************************************* -}

arityLam :: Id -> ArityType -> ArityType
arityLam id (AT oss div)
  = AT ((IsCheap, one_shot) : oss) div
  where
    one_shot | isDeadEndDiv div = OneShotLam
             | otherwise        = idStateHackOneShotInfo id
    -- If the body diverges, treat it as one-shot: no point
    -- in floating out, and no penalty for floating in
    -- See Wrinkle [Bottoming functions] in Note [ArityType]

floatIn :: Cost -> ArityType -> ArityType
-- We have something like (let x = E in b),
-- where b has the given arity type.
floatIn IsCheap     at = at
floatIn IsExpensive at = addWork at

addWork :: ArityType -> ArityType
addWork at@(AT lams div)
  = case lams of
      []      -> at
      lam:lams' -> AT (add_work lam : lams') div
  where
    add_work :: ATLamInfo -> ATLamInfo
    add_work (_,os) = (IsExpensive,os)

arityApp :: ArityType -> Cost -> ArityType
-- Processing (fun arg) where at is the ArityType of fun,
-- Knock off an argument and behave like 'let'
arityApp (AT ((ch1,_):oss) div) ch2 = floatIn (ch1 `addCost` ch2) (AT oss div)
arityApp at                     _   = at

-- | Least upper bound in the 'ArityType' lattice.
-- See the haddocks on 'ArityType' for the lattice.
--
-- Used for branches of a @case@.
andArityType :: ArityType -> ArityType -> ArityType
andArityType (AT (lam1:lams1) div1) (AT (lam2:lams2) div2)
  | AT lams' div' <- andArityType (AT lams1 div1) (AT lams2 div2)
  = AT ((lam1 `and_lam` lam2) : lams') div' -- See Note [Combining case branches]
  where
    (ch1,os1) `and_lam` (ch2,os2)
      = ( ch1 `addCost` ch2, os1 `bestOneShot` os2)

andArityType (AT [] div1) at2 = andWithTail div1 at2
andArityType at1 (AT [] div2) = andWithTail div2 at1

andWithTail :: Divergence -> ArityType -> ArityType
andWithTail div1 at2@(AT oss2 _)
  | isDeadEndDiv div1     -- case x of { T -> error; F -> \y.e }
  = at2
  | otherwise  -- case x of { T -> plusInt <expensive>; F -> \y.e }
  = addWork (AT oss2 topDiv)   -- We know div1 = topDiv
        -- Note [ABot branches: max arity wins]
        -- See Note [Combining case branches]

{- Note [ABot branches: max arity wins]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider   case x of
             True  -> \x.  error "urk"
             False -> \xy. error "urk2"

Remember: \o1..on.⊥ means "if you apply to n args, it'll definitely diverge".
So we need \??.⊥ for the whole thing, the /max/ of both arities.

Note [Combining case branches]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  go = \x. let z = go e0
               go2 = \x. case x of
                           True  -> z
                           False -> \s(one-shot). e1
           in go2 x
We *really* want to respect the one-shot annotation provided by the
user and eta-expand go and go2.
When combining the branches of the case we have
     T `andAT` \1.T
and we want to get \1.T.
But if the inner lambda wasn't one-shot (\?.T) we don't want to do this.
(We need a usage analysis to justify that.)

So we combine the best of the two branches, on the (slightly dodgy)
basis that if we know one branch is one-shot, then they all must be.
Surprisingly, this means that the one-shot arity type is effectively the top
element of the lattice.

Note [Eta expanding through CallStacks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Just as it's good to eta-expand through dictionaries, so it is good to
do so through CallStacks.  #20103 is a case in point, where we got
  foo :: HasCallStack => Int -> Int
  foo = \(d::CallStack). let d2 = pushCallStack blah d in
        \(x:Int). blah

We really want to eta-expand this!  #20103 is quite convincing!
We do this regardless of -fdicts-cheap; it's not really a dictionary.

Note [Eta expanding through dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the experimental -fdicts-cheap flag is on, we eta-expand through
dictionary bindings.  This improves arities. Thereby, it also
means that full laziness is less prone to floating out the
application of a function to its dictionary arguments, which
can thereby lose opportunities for fusion.  Example:
        foo :: Ord a => a -> ...
     foo = /\a \(d:Ord a). let d' = ...d... in \(x:a). ....
        -- So foo has arity 1

     f = \x. foo dInt $ bar x

The (foo DInt) is floated out, and makes ineffective a RULE
     foo (bar x) = ...

One could go further and make exprIsCheap reply True to any
dictionary-typed expression, but that's more work.
-}

---------------------------

-- | Each of the entry-points of the analyser ('arityType') has different
-- requirements. The entry-points are
--
--   1. 'exprBotStrictness_maybe'
--   2. 'exprEtaExpandArity'
--   3. 'findRhsArity'
--
-- For each of the entry-points, there is a separate mode that governs
--
--   1. How pedantic we are wrt. ⊥, in 'pedanticBottoms'.
--   2. Whether we store arity signatures for non-recursive let-bindings,
--      accessed in 'extendSigEnv'/'lookupSigEnv'.
--      See Note [Arity analysis] why that's important.
--   3. Which expressions we consider cheap to float inside a lambda,
--      in 'myExprIsCheap'.
data AnalysisMode
  = BotStrictness
  -- ^ Used during 'exprBotStrictness_maybe'.

  | EtaExpandArity { am_ped_bot :: !Bool
                   , am_dicts_cheap :: !Bool }
  -- ^ Used for finding an expression's eta-expanding arity quickly,
  -- without fixed-point iteration ('exprEtaExpandArity').

  | FindRhsArity { am_ped_bot     :: !Bool
                 , am_dicts_cheap :: !Bool
                 , am_sigs        :: !(IdEnv SafeArityType) }
  -- ^ Used for regular, fixed-point arity analysis ('findRhsArity').
  --   See Note [Arity analysis] for details about fixed-point iteration.
  -- am_dicts_cheap: see Note [Eta expanding through dictionaries]
  -- am_sigs: note `SafeArityType` so we can use this in myIsCheapApp
  -- INVARIANT: am_sigs is disjoint with 'ae_joins'.

data ArityEnv
  = AE
  { ae_mode   :: !AnalysisMode
  -- ^ The analysis mode. See 'AnalysisMode'.
  , ae_joins  :: !IdSet
  -- ^ In-scope join points. See Note [Eta-expansion and join points]
  --   INVARIANT: Disjoint with the domain of 'am_sigs' (if present).
  }

-- | The @ArityEnv@ used by 'exprBotStrictness_maybe'. Pedantic about bottoms
-- and no application is ever considered cheap.
botStrictnessArityEnv :: ArityEnv
botStrictnessArityEnv = AE { ae_mode = BotStrictness, ae_joins = emptyVarSet }

-- | The @ArityEnv@ used by 'exprEtaExpandArity'.
etaExpandArityEnv :: DynFlags -> ArityEnv
etaExpandArityEnv dflags
  = AE { ae_mode  = EtaExpandArity { am_ped_bot = gopt Opt_PedanticBottoms dflags
                                   , am_dicts_cheap = gopt Opt_DictsCheap dflags }
       , ae_joins = emptyVarSet }

-- | The @ArityEnv@ used by 'findRhsArity'.
findRhsArityEnv :: DynFlags -> ArityEnv
findRhsArityEnv dflags
  = AE { ae_mode  = FindRhsArity { am_ped_bot = gopt Opt_PedanticBottoms dflags
                                 , am_dicts_cheap = gopt Opt_DictsCheap dflags
                                 , am_sigs = emptyVarEnv }
       , ae_joins = emptyVarSet }

-- First some internal functions in snake_case for deleting in certain VarEnvs
-- of the ArityType. Don't call these; call delInScope* instead!

modifySigEnv :: (IdEnv ArityType -> IdEnv ArityType) -> ArityEnv -> ArityEnv
modifySigEnv f env@AE { ae_mode = am@FindRhsArity{am_sigs = sigs} } =
  env { ae_mode = am { am_sigs = f sigs } }
modifySigEnv _ env = env
{-# INLINE modifySigEnv #-}

del_sig_env :: Id -> ArityEnv -> ArityEnv -- internal!
del_sig_env id = modifySigEnv (\sigs -> delVarEnv sigs id)
{-# INLINE del_sig_env #-}

del_sig_env_list :: [Id] -> ArityEnv -> ArityEnv -- internal!
del_sig_env_list ids = modifySigEnv (\sigs -> delVarEnvList sigs ids)
{-# INLINE del_sig_env_list #-}

del_join_env :: JoinId -> ArityEnv -> ArityEnv -- internal!
del_join_env id env@(AE { ae_joins = joins })
  = env { ae_joins = delVarSet joins id }
{-# INLINE del_join_env #-}

del_join_env_list :: [JoinId] -> ArityEnv -> ArityEnv -- internal!
del_join_env_list ids env@(AE { ae_joins = joins })
  = env { ae_joins = delVarSetList joins ids }
{-# INLINE del_join_env_list #-}

-- end of internal deletion functions

extendJoinEnv :: ArityEnv -> [JoinId] -> ArityEnv
extendJoinEnv env@(AE { ae_joins = joins }) join_ids
  = del_sig_env_list join_ids
  $ env { ae_joins = joins `extendVarSetList` join_ids }

extendSigEnv :: ArityEnv -> Id -> SafeArityType -> ArityEnv
extendSigEnv env id ar_ty
  = del_join_env id                                    $
    modifySigEnv (\sigs -> extendVarEnv sigs id ar_ty) $
    env

delInScope :: ArityEnv -> Id -> ArityEnv
delInScope env id = del_join_env id $ del_sig_env id env

delInScopeList :: ArityEnv -> [Id] -> ArityEnv
delInScopeList env ids = del_join_env_list ids $ del_sig_env_list ids env

lookupSigEnv :: ArityEnv -> Id -> Maybe SafeArityType
lookupSigEnv AE{ ae_mode = mode } id = case mode of
  BotStrictness                  -> Nothing
  EtaExpandArity{}               -> Nothing
  FindRhsArity{ am_sigs = sigs } -> lookupVarEnv sigs id

-- | Whether the analysis should be pedantic about bottoms.
-- 'exprBotStrictness_maybe' always is.
pedanticBottoms :: ArityEnv -> Bool
pedanticBottoms AE{ ae_mode = mode } = case mode of
  BotStrictness                          -> True
  EtaExpandArity{ am_ped_bot = ped_bot } -> ped_bot
  FindRhsArity{ am_ped_bot = ped_bot }   -> ped_bot

exprCost :: ArityEnv -> CoreExpr -> Maybe Type -> Cost
exprCost env e mb_ty
  | myExprIsCheap env e mb_ty = IsCheap
  | otherwise                 = IsExpensive

-- | A version of 'exprIsCheap' that considers results from arity analysis
-- and optionally the expression's type.
-- Under 'exprBotStrictness_maybe', no expressions are cheap.
myExprIsCheap :: ArityEnv -> CoreExpr -> Maybe Type -> Bool
myExprIsCheap AE{ae_mode = mode} e mb_ty = case mode of
  BotStrictness -> False
  _             -> cheap_dict || cheap_fun e
    where
      cheap_dict = case mb_ty of
                     Nothing -> False
                     Just ty -> (am_dicts_cheap mode && isDictTy ty)
                                || isCallStackPredTy ty
        -- See Note [Eta expanding through dictionaries]
        -- See Note [Eta expanding through CallStacks]

      cheap_fun e = case mode of
#if __GLASGOW_HASKELL__ <= 900
        BotStrictness                -> panic "impossible"
#endif
        EtaExpandArity{}             -> exprIsCheap e
        FindRhsArity{am_sigs = sigs} -> exprIsCheapX (myIsCheapApp sigs) e

-- | A version of 'isCheapApp' that considers results from arity analysis.
-- See Note [Arity analysis] for what's in the signature environment and why
-- it's important.
myIsCheapApp :: IdEnv SafeArityType -> CheapAppFun
myIsCheapApp sigs fn n_val_args = case lookupVarEnv sigs fn of

  -- Nothing means not a local function, fall back to regular
  -- 'GHC.Core.Utils.isCheapApp'
  Nothing -> isCheapApp fn n_val_args

  -- `Just at` means local function with `at` as current SafeArityType.
  -- Roughly approximate what 'isCheapApp' is doing.
  Just (AT lams div)
    | isDeadEndDiv div -> True -- See Note [isCheapApp: bottoming functions] in GHC.Core.Utils
    | n_val_args == 0          -> True -- Essentially
    | n_val_args < length lams -> True -- isWorkFreeApp
    | otherwise                -> False

----------------
arityType :: ArityEnv -> CoreExpr -> ArityType

arityType env (Var v)
  | v `elemVarSet` ae_joins env
  = botArityType  -- See Note [Eta-expansion and join points]
  | Just at <- lookupSigEnv env v -- Local binding
  = at
  | otherwise
  = idArityType v

arityType env (Cast e _)
  = arityType env e

        -- Lambdas; increase arity
arityType env (Lam x e)
  | isId x    = arityLam x (arityType env' e)
  | otherwise = arityType env' e
  where
    env' = delInScope env x

        -- Applications; decrease arity, except for types
arityType env (App fun (Type _))
   = arityType env fun
arityType env (App fun arg )
   = arityApp fun_at arg_cost
   where
     fun_at   = arityType env fun
     arg_cost = exprCost env arg Nothing

        -- Case/Let; keep arity if either the expression is cheap
        -- or it's a 1-shot lambda
        -- The former is not really right for Haskell
        --      f x = case x of { (a,b) -> \y. e }
        --  ===>
        --      f x y = case x of { (a,b) -> e }
        -- The difference is observable using 'seq'
        --
arityType env (Case scrut bndr _ alts)
  | exprIsDeadEnd scrut || null alts
  = botArityType    -- Do not eta expand. See (1) in Note [Dealing with bottom]

  | not (pedanticBottoms env)  -- See (2) in Note [Dealing with bottom]
  , myExprIsCheap env scrut (Just (idType bndr))
  = alts_type

  | exprOkForSpeculation scrut
  = alts_type

  | otherwise            -- In the remaining cases we may not push
  = addWork alts_type -- evaluation of the scrutinee in
  where
    env' = delInScope env bndr
    arity_type_alt (Alt _con bndrs rhs) = arityType (delInScopeList env' bndrs) rhs
    alts_type = foldr1 andArityType (map arity_type_alt alts)

arityType env (Let (NonRec j rhs) body)
  | Just join_arity <- isJoinId_maybe j
  , (_, rhs_body)   <- collectNBinders join_arity rhs
  = -- See Note [Eta-expansion and join points]
    andArityType (arityType env rhs_body)
                 (arityType env' body)
  where
     env' = extendJoinEnv env [j]

arityType env (Let (Rec pairs) body)
  | ((j,_):_) <- pairs
  , isJoinId j
  = -- See Note [Eta-expansion and join points]
    foldr (andArityType . do_one) (arityType env' body) pairs
  where
    env' = extendJoinEnv env (map fst pairs)
    do_one (j,rhs)
      | Just arity <- isJoinId_maybe j
      = arityType env' $ snd $ collectNBinders arity rhs
      | otherwise
      = pprPanic "arityType:joinrec" (ppr pairs)

arityType env (Let (NonRec b rhs) e)
  = floatIn rhs_cost (arityType env' e)
  where
    rhs_cost = exprCost env rhs (Just (idType b))
    env'     = extendSigEnv env b (safeArityType (arityType env rhs))

arityType env (Let (Rec prs) e)
  = floatIn (allCosts bind_cost prs) (arityType env' e)
  where
    env'            = delInScopeList env (map fst prs)
    bind_cost (b,e) = exprCost env' e (Just (idType b))

arityType env (Tick t e)
  | not (tickishIsCode t)     = arityType env e

arityType _ _ = topArityType

{- Note [Eta-expansion and join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#18328)

  f x = join j y = case y of
                      True -> \a. blah
                      False -> \b. blah
        in case x of
              A -> j True
              B -> \c. blah
              C -> j False

and suppose the join point is too big to inline.  Now, what is the
arity of f?  If we inlined the join point, we'd definitely say "arity
2" because we are prepared to push case-scrutinisation inside a
lambda.  But currently the join point totally messes all that up,
because (thought of as a vanilla let-binding) the arity pinned on 'j'
is just 1.

Why don't we eta-expand j?  Because of
Note [Do not eta-expand join points] in GHC.Core.Opt.Simplify.Utils

Even if we don't eta-expand j, why is its arity only 1?
See invariant 2b in Note [Invariants on join points] in GHC.Core.

So we do this:

* Treat the RHS of a join-point binding, /after/ stripping off
  join-arity lambda-binders, as very like the body of the let.
  More precisely, do andArityType with the arityType from the
  body of the let.

* Dually, when we come to a /call/ of a join point, just no-op
  by returning ABot, the bottom element of ArityType,
  which so that: bot `andArityType` x = x

* This works if the join point is bound in the expression we are
  taking the arityType of.  But if it's bound further out, it makes
  no sense to say that (say) the arityType of (j False) is ABot.
  Bad things happen.  So we keep track of the in-scope join-point Ids
  in ae_join.

This will make f, above, have arity 2. Then, we'll eta-expand it thus:

  f x eta = (join j y = ... in case x of ...) eta

and the Simplify will automatically push that application of eta into
the join points.

An alternative (roughly equivalent) idea would be to carry an
environment mapping let-bound Ids to their ArityType.
-}

idArityType :: Id -> ArityType
idArityType v
  | strict_sig <- idDmdSig v
  , not $ isNopSig strict_sig
  , (ds, div) <- splitDmdSig strict_sig
  , let arity = length ds
  -- Every strictness signature admits an arity signature!
  = AT (take arity one_shots) div
  | otherwise
  = AT (take (idArity v) one_shots) topDiv
  where
    one_shots :: [(Cost,OneShotInfo)]  -- One-shot-ness derived from the type
    one_shots = repeat IsCheap `zip` typeOneShots (idType v)

{-
%************************************************************************
%*                                                                      *
              The main eta-expander
%*                                                                      *
%************************************************************************

We go for:
   f = \x1..xn -> N  ==>   f = \x1..xn y1..ym -> N y1..ym
                                 (n >= 0)

where (in both cases)

        * The xi can include type variables

        * The yi are all value variables

        * N is a NORMAL FORM (i.e. no redexes anywhere)
          wanting a suitable number of extra args.

The biggest reason for doing this is for cases like

        f = \x -> case x of
                    True  -> \y -> e1
                    False -> \y -> e2

Here we want to get the lambdas together.  A good example is the nofib
program fibheaps, which gets 25% more allocation if you don't do this
eta-expansion.

We may have to sandwich some coerces between the lambdas
to make the types work.   exprEtaExpandArity looks through coerces
when computing arity; and etaExpand adds the coerces as necessary when
actually computing the expansion.

Note [No crap in eta-expanded code]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The eta expander is careful not to introduce "crap".  In particular,
given a CoreExpr satisfying the 'CpeRhs' invariant (in CorePrep), it
returns a CoreExpr satisfying the same invariant. See Note [Eta
expansion and the CorePrep invariants] in CorePrep.

This means the eta-expander has to do a bit of on-the-fly
simplification but it's not too hard.  The alternative, of relying on
a subsequent clean-up phase of the Simplifier to de-crapify the result,
means you can't really use it in CorePrep, which is painful.

Note [Eta expansion for join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The no-crap rule is very tiresome to guarantee when
we have join points. Consider eta-expanding
   let j :: Int -> Int -> Bool
       j x = e
   in b

The simple way is
  \(y::Int). (let j x = e in b) y

The no-crap way is
  \(y::Int). let j' :: Int -> Bool
                 j' x = e y
             in b[j'/j] y
where I have written to stress that j's type has
changed.  Note that (of course!) we have to push the application
inside the RHS of the join as well as into the body.  AND if j
has an unfolding we have to push it into there too.  AND j might
be recursive...

So for now I'm abandoning the no-crap rule in this case. I think
that for the use in CorePrep it really doesn't matter; and if
it does, then CoreToStg.myCollectArgs will fall over.

(Moreover, I think that casts can make the no-crap rule fail too.)

Note [Eta expansion and SCCs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that SCCs are not treated specially by etaExpand.  If we have
        etaExpand 2 (\x -> scc "foo" e)
        = (\xy -> (scc "foo" e) y)
So the costs of evaluating 'e' (not 'e y') are attributed to "foo"

Note [Eta expansion and source notes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CorePrep puts floatable ticks outside of value applications, but not
type applications. As a result we might be trying to eta-expand an
expression like

  (src<...> v) @a

which we want to lead to code like

  \x -> src<...> v @a x

This means that we need to look through type applications and be ready
to re-add floats on the top.

Note [Eta expansion with ArityType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The etaExpandAT function takes an ArityType (not just an Arity) to
guide eta-expansion.  Why? Because we want to preserve one-shot info.
Consider
  foo = \x. case x of
              True  -> (\s{os}. blah) |> co
              False -> wubble
We'll get an ArityType for foo of \?1.T.

Then we want to eta-expand to
  foo = (\x. \eta{os}. (case x of ...as before...) eta) |> some_co

That 'eta' binder is fresh, and we really want it to have the
one-shot flag from the inner \s{os}.  By expanding with the
ArityType gotten from analysing the RHS, we achieve this neatly.

This makes a big difference to the one-shot monad trick;
see Note [The one-shot state monad trick] in GHC.Utils.Monad.
-}

-- | @etaExpand n e@ returns an expression with
-- the same meaning as @e@, but with arity @n@.
--
-- Given:
--
-- > e' = etaExpand n e
--
-- We should have that:
--
-- > ty = exprType e = exprType e'

etaExpand :: Arity -> CoreExpr -> CoreExpr
etaExpand n orig_expr
  = eta_expand in_scope (replicate n NoOneShotInfo) orig_expr
  where
    in_scope = {-#SCC "eta_expand:in-scopeX" #-}
               mkInScopeSet (exprFreeVars orig_expr)

etaExpandAT :: InScopeSet -> SafeArityType -> CoreExpr -> CoreExpr
-- See Note [Eta expansion with ArityType]
--
-- We pass in the InScopeSet from the simplifier to avoid recomputing
-- it here, which can be jolly expensive if the casts are big
-- In #18223 it took 10% of compile time just to do the exprFreeVars!
etaExpandAT in_scope at orig_expr
  = eta_expand in_scope (arityTypeOneShots at) orig_expr

-- etaExpand arity e = res
-- Then 'res' has at least 'arity' lambdas at the top
--    possibly with a cast wrapped around the outside
-- See Note [Eta expansion with ArityType]
--
-- etaExpand deals with for-alls. For example:
--              etaExpand 1 E
-- where  E :: forall a. a -> a
-- would return
--      (/\b. \y::a -> E b y)

eta_expand :: InScopeSet -> [OneShotInfo] -> CoreExpr -> CoreExpr
eta_expand in_scope one_shots (Cast expr co)
  = mkCast (eta_expand in_scope one_shots expr) co
    -- This mkCast is important, because eta_expand might return an
    -- expression with a cast at the outside; and tryCastWorkerWrapper
    -- asssumes that we don't have nested casts. Makes a difference
    -- in compile-time for T18223

eta_expand in_scope one_shots orig_expr
  = go in_scope one_shots [] orig_expr
  where
      -- Strip off existing lambdas and casts before handing off to mkEtaWW
      -- This is mainly to avoid spending time cloning binders and substituting
      -- when there is actually nothing to do.  It's slightly awkward to deal
      -- with casts here, apart from the topmost one, and they are rare, so
      -- if we find one we just hand off to mkEtaWW anyway
      -- Note [Eta expansion and SCCs]
    go _ [] _ _ = orig_expr  -- Already has the specified arity; no-op

    go in_scope oss@(_:oss1) vs (Lam v body)
      | isTyVar v = go (in_scope `extendInScopeSet` v) oss  (v:vs) body
      | otherwise = go (in_scope `extendInScopeSet` v) oss1 (v:vs) body

    go in_scope oss rev_vs expr
      = -- pprTrace "ee" (vcat [ppr in_scope', ppr top_bndrs, ppr eis]) $
        retick $
        etaInfoAbs top_eis $
        etaInfoApp in_scope' sexpr eis
      where
          (in_scope', eis@(EI eta_bndrs mco))
                    = mkEtaWW oss (ppr orig_expr) in_scope (exprType expr)
          top_bndrs = reverse rev_vs
          top_eis   = EI (top_bndrs ++ eta_bndrs) (mkPiMCos top_bndrs mco)

          -- Find ticks behind type apps.
          -- See Note [Eta expansion and source notes]
          -- I don't really understand this code SLPJ May 21
          (expr', args) = collectArgs expr
          (ticks, expr'') = stripTicksTop tickishFloatable expr'
          sexpr = mkApps expr'' args
          retick expr = foldr mkTick expr ticks

{- *********************************************************************
*                                                                      *
              The EtaInfo mechanism
          mkEtaWW, etaInfoAbs, etaInfoApp
*                                                                      *
********************************************************************* -}

{- Note [The EtaInfo mechanism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have (e :: ty) and we want to eta-expand it to arity N.
This what eta_expand does.  We do it in two steps:

1.  mkEtaWW: from 'ty' and 'N' build a EtaInfo which describes
    the shape of the expansion necessary to expand to arity N.

2.  Build the term
       \ v1..vn.  e v1 .. vn
    where those abstractions and applications are described by
    the same EtaInfo.  Specifically we build the term

       etaInfoAbs etas (etaInfoApp in_scope e etas)

   where etas :: EtaInfo
         etaInfoAbs builds the lambdas
         etaInfoApp builds the applictions

   Note that the /same/ EtaInfo drives both etaInfoAbs and etaInfoApp

To a first approximation EtaInfo is just [Var].  But
casts complicate the question.  If we have
   newtype N a = MkN (S -> a)
     axN :: N a  ~  S -> a
and
   e :: N (N Int)
then the eta-expansion should look like
   (\(x::S) (y::S) -> (e |> co) x y) |> sym co
where
  co :: N (N Int) ~ S -> S -> Int
  co = axN @(N Int) ; (S -> axN @Int)

We want to get one cast, at the top, to account for all those
nested newtypes. This is expressed by the EtaInfo type:

   data EtaInfo = EI [Var] MCoercionR

Note [Check for reflexive casts in eta expansion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It turns out that the casts created by teh above mechanism are often Refl.
When casts are very deeply nested (as happens in #18223), the repetition
of types can make the overall term very large.  So there is a big
payoff in cancelling out casts aggressively wherever possible.
(See also Note [No crap in eta-expanded code].)

This matters particularly in etaInfoApp, where we
* Do beta-reduction on the fly
* Use getArg_maybe to get a cast out of the way,
  so that we can do beta reduction
Together this makes a big difference.  Consider when e is
   case x of
      True  -> (\x -> e1) |> c1
      False -> (\p -> e2) |> c2

When we eta-expand this to arity 1, say, etaInfoAbs will wrap
a (\eta) around the outside and use etaInfoApp to apply each
alternative to 'eta'.  We want to beta-reduce all that junk
away.

#18223 was a dramatic example in which the intermediate term was
grotesquely huge, even though the next Simplifier iteration squashed
it.  Better to kill it at birth.

The crucial spots in etaInfoApp are:
* `checkReflexiveMCo` in the (Cast e co) case of `go`
* `checkReflexiveMCo` in `pushCoArg`
* Less important: checkReflexiveMCo in the final case of `go`
Collectively these make a factor-of-5 difference to the total
allocation of T18223, so take care if you change this stuff!

Example:
   newtype N = MkN (Y->Z)
   f :: X -> N
   f = \(x::X). ((\(y::Y). blah) |> fco)

where fco :: (Y->Z) ~ N

mkEtaWW makes an EtaInfo of (EI [(eta1:X), (eta2:Y)] eta_co
  where
    eta_co :: (X->N) ~ (X->Y->Z)
    eta_co =  (<X> -> nco)
    nco :: N ~ (Y->Z)  -- Comes from topNormaliseNewType_maybe

Now, when we push that eta_co inward in etaInfoApp:
* In the (Cast e co) case, the 'fco' and 'nco' will meet, and
  should cancel.
* When we meet the (\y.e) we want no cast on the y.

-}

--------------
data EtaInfo = EI [Var] MCoercionR

-- (EI bs co) describes a particular eta-expansion, as follows:
--  Abstraction:  (\b1 b2 .. bn. []) |> sym co
--  Application:  ([] |> co) b1 b2 .. bn
--
--    e :: T    co :: T ~ (t1 -> t2 -> .. -> tn -> tr)
--    e = (\b1 b2 ... bn. (e |> co) b1 b2 .. bn) |> sym co

instance Outputable EtaInfo where
  ppr (EI vs mco) = text "EI" <+> ppr vs <+> parens (ppr mco)


etaInfoApp :: InScopeSet -> CoreExpr -> EtaInfo -> CoreExpr
-- (etaInfoApp s e (EI bs mco) returns something equivalent to
--             ((substExpr s e) |> mco b1 .. bn)
-- See Note [The EtaInfo mechanism]
--
-- NB: With very deeply nested casts, this function can be expensive
--     In T18223, this function alone costs 15% of allocation, all
--     spent in the calls to substExprSC and substBindSC

etaInfoApp in_scope expr eis
  = go (mkEmptySubst in_scope) expr eis
  where
    go :: Subst -> CoreExpr -> EtaInfo -> CoreExpr
    -- 'go' pushed down the eta-infos into the branch of a case
    -- and the body of a let; and does beta-reduction if possible
    --   go subst fun co [b1,..,bn]  returns  (subst(fun) |> co) b1 .. bn
    go subst (Tick t e) eis
      = Tick (substTickish subst t) (go subst e eis)

    go subst (Cast e co) (EI bs mco)
      = go subst e (EI bs mco')
      where
        mco' = checkReflexiveMCo (Core.substCo subst co `mkTransMCoR` mco)
               -- See Note [Check for reflexive casts in eta expansion]

    go subst (Case e b ty alts) eis
      = Case (Core.substExprSC subst e) b1 ty' alts'
      where
        (subst1, b1) = Core.substBndr subst b
        alts' = map subst_alt alts
        ty'   = etaInfoAppTy (Core.substTy subst ty) eis
        subst_alt (Alt con bs rhs) = Alt con bs' (go subst2 rhs eis)
                 where
                  (subst2,bs') = Core.substBndrs subst1 bs

    go subst (Let b e) eis
      | not (isJoinBind b) -- See Note [Eta expansion for join points]
      = Let b' (go subst' e eis)
      where
        (subst', b') = Core.substBindSC subst b

    -- Beta-reduction if possible, pushing any intervening casts past
    -- the argument. See Note [The EtaInfo mechanism]
    go subst (Lam v e) (EI (b:bs) mco)
      | Just (arg,mco') <- pushMCoArg mco (varToCoreExpr b)
      = go (Core.extendSubst subst v arg) e (EI bs mco')

    -- Stop pushing down; just wrap the expression up
    -- See Note [Check for reflexive casts in eta expansion]
    go subst e (EI bs mco) = Core.substExprSC subst e
                             `mkCastMCo` checkReflexiveMCo mco
                             `mkVarApps` bs

--------------
etaInfoAppTy :: Type -> EtaInfo -> Type
-- If                    e :: ty
-- then   etaInfoApp e eis :: etaInfoApp ty eis
etaInfoAppTy ty (EI bs mco)
  = applyTypeToArgs (text "etaInfoAppTy") ty1 (map varToCoreExpr bs)
  where
    ty1 = case mco of
             MRefl  -> ty
             MCo co -> coercionRKind co

--------------
etaInfoAbs :: EtaInfo -> CoreExpr -> CoreExpr
-- See Note [The EtaInfo mechanism]
etaInfoAbs (EI bs mco) expr = (mkLams bs expr) `mkCastMCo` mkSymMCo mco

--------------
-- | @mkEtaWW n _ fvs ty@ will compute the 'EtaInfo' necessary for eta-expanding
-- an expression @e :: ty@ to take @n@ value arguments, where @fvs@ are the
-- free variables of @e@.
--
-- Note that this function is entirely unconcerned about cost centres and other
-- semantically-irrelevant source annotations, so call sites must take care to
-- preserve that info. See Note [Eta expansion and SCCs].
mkEtaWW
  :: [OneShotInfo]
  -- ^ How many value arguments to eta-expand
  -> SDoc
  -- ^ The pretty-printed original expression, for warnings.
  -> InScopeSet
  -- ^ A super-set of the free vars of the expression to eta-expand.
  -> Type
  -> (InScopeSet, EtaInfo)
  -- ^ The variables in 'EtaInfo' are fresh wrt. to the incoming 'InScopeSet'.
  -- The outgoing 'InScopeSet' extends the incoming 'InScopeSet' with the
  -- fresh variables in 'EtaInfo'.

mkEtaWW orig_oss ppr_orig_expr in_scope orig_ty
  = go 0 orig_oss empty_subst orig_ty
  where
    empty_subst = mkEmptyTCvSubst in_scope

    go :: Int                -- For fresh names
       -> [OneShotInfo]      -- Number of value args to expand to
       -> TCvSubst -> Type   -- We are really looking at subst(ty)
       -> (InScopeSet, EtaInfo)
    -- (go [o1,..,on] subst ty) = (in_scope, EI [b1,..,bn] co)
    --    co :: subst(ty) ~ b1_ty -> ... -> bn_ty -> tr

    go _ [] subst _
       ----------- Done!  No more expansion needed
       = (getTCvInScope subst, EI [] MRefl)

    go n oss@(one_shot:oss1) subst ty
       ----------- Forall types  (forall a. ty)
       | Just (tcv,ty') <- splitForAllTyCoVar_maybe ty
       , (subst', tcv') <- Type.substVarBndr subst tcv
       , let oss' | isTyVar tcv = oss
                  | otherwise   = oss1
         -- A forall can bind a CoVar, in which case
         -- we consume one of the [OneShotInfo]
       , (in_scope, EI bs mco) <- go n oss' subst' ty'
       = (in_scope, EI (tcv' : bs) (mkHomoForAllMCo tcv' mco))

       ----------- Function types  (t1 -> t2)
       | Just (mult, arg_ty, res_ty) <- splitFunTy_maybe ty
       , typeHasFixedRuntimeRep arg_ty
          -- See Note [Representation polymorphism invariants] in GHC.Core
          -- See also test case typecheck/should_run/EtaExpandLevPoly

       , (subst', eta_id) <- freshEtaId n subst (Scaled mult arg_ty)
          -- Avoid free vars of the original expression

       , let eta_id' = eta_id `setIdOneShotInfo` one_shot
       , (in_scope, EI bs mco) <- go (n+1) oss1 subst' res_ty
       = (in_scope, EI (eta_id' : bs) (mkFunResMCo (idScaledType eta_id') mco))

       ----------- Newtypes
       -- Given this:
       --      newtype T = MkT ([T] -> Int)
       -- Consider eta-expanding this
       --      eta_expand 1 e T
       -- We want to get
       --      coerce T (\x::[T] -> (coerce ([T]->Int) e) x)
       | Just (co, ty') <- topNormaliseNewType_maybe ty
       , -- co :: ty ~ ty'
         let co' = Type.substCo subst co
             -- Remember to apply the substitution to co (#16979)
             -- (or we could have applied to ty, but then
             --  we'd have had to zap it for the recursive call)
       , (in_scope, EI bs mco) <- go n oss subst ty'
         -- mco :: subst(ty') ~ b1_ty -> ... -> bn_ty -> tr
       = (in_scope, EI bs (mkTransMCoR co' mco))

       | otherwise       -- We have an expression of arity > 0,
                         -- but its type isn't a function, or a binder
                         -- does not have a fixed runtime representation
       = warnPprTrace True "mkEtaWW" ((ppr orig_oss <+> ppr orig_ty) $$ ppr_orig_expr)
         (getTCvInScope subst, EI [] MRefl)
        -- This *can* legitimately happen:
        -- e.g.  coerce Int (\x. x) Essentially the programmer is
        -- playing fast and loose with types (Happy does this a lot).
        -- So we simply decline to eta-expand.  Otherwise we'd end up
        -- with an explicit lambda having a non-function type


{-
************************************************************************
*                                                                      *
                Eta reduction
*                                                                      *
************************************************************************

Note [Eta reduction makes sense]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's eta reduction transforms
   \x y. <fun> x y  --->  <fun>
We discuss when this is /sound/ in Note [Eta reduction soundness].
But even assuming it is sound, when is it /desirable/.  That
is what we discuss here.

This test is made by `ok_fun` in tryEtaReduce.

1. We want to eta-reduce only if we get all the way to a trivial
   expression; we don't want to remove extra lambdas unless we are
   going to avoid allocating this thing altogether.

   Trivial means *including* casts and type lambdas:
     * `\x. f x |> co  -->  f |> (ty(x) -> co)` (provided `co` doesn't mention `x`)
     * `/\a. \x. f @(Maybe a) x -->  /\a. f @(Maybe a)`
   See Note [Do not eta reduce PAPs] for why we insist on a trivial head.

2. Type and dictionary abstraction. Regardless of whether 'f' is a value, it
   is always sound to reduce /type lambdas/, thus:
        (/\a -> f a)  -->   f
   Moreover, we always want to, because it makes RULEs apply more often:
      This RULE:    `forall g. foldr (build (/\a -> g a))`
      should match  `foldr (build (/\b -> ...something complex...))`
   and the simplest way to do so is eta-reduce `/\a -> g a` in the RULE to `g`.

   The type checker can insert these eta-expanded versions,
   with both type and dictionary lambdas; hence the slightly
   ad-hoc (all ok_lam bndrs)

3. (See fun_arity in tryEtaReduce.) We have to hide `f`'s `idArity` in
   its own RHS, lest we suffer from the last point of Note [Arity
   robustness] in GHC.Core.Opt.Simplify.Env. There we have `f = \x. f x`
   and we should not eta-reduce to `f=f`. Which might change a
   terminating program (think @f `seq` e@) to a non-terminating one.
   So we check for being a loop breaker first. However for GlobalIds
   we can look at the arity; and for primops we must, since they have
   no unfolding.  [SG: Perhaps this is rather a soundness subtlety?]

Of course, eta reduction is not always sound. See Note [Eta reduction soundness]
for when it is.

When there are multiple arguments, we might get multiple eta-redexes. Example:
   \x y. e x y
   ==> { reduce \y. (e x) y in context \x._ }
   \x. e x
   ==> { reduce \x. e x in context _ }
   e
And (1) implies that we never want to stop with `\x. e x`, because that is not a
trivial expression. So in practice, the implementation works by considering a
whole group of leading lambdas to reduce.

These delicacies are why we don't simply use 'exprIsTrivial' and 'exprIsHNF'
in 'tryEtaReduce'. Alas.

Note [Eta reduction soundness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's eta reduction transforms
   \x y. <fun> x y  --->  <fun>
For soundness, we obviously require that `x` and `y`
to not occur free. But what /other/ restrictions are there for
eta reduction to be sound?

We discuss separately what it means for eta reduction to be
/desirable/, in Note [Eta reduction makes sense].

Eta reduction is *not* a sound transformation in general, because it
may change termination behavior if *value* lambdas are involved:
  `bot`  /=  `\x. bot x`   (as can be observed by a simple `seq`)
The past has shown that oversight of this fact can not only lead to endless
loops or exceptions, but also straight out *segfaults*.

Nevertheless, we can give the following criteria for when it is sound to
perform eta reduction on an expression with n leading lambdas `\xs. e xs`
(checked in 'is_eta_reduction_sound' in 'tryEtaReduce', which focuses on the
case where `e` is trivial):

 A. It is sound to eta-reduce n arguments as long as n does not exceed the
    `exprArity` of `e`. (Needs Arity analysis.)
    This criterion exploits information about how `e` is *defined*.

    Example: If `e = \x. bot` then we know it won't diverge until it is called
    with one argument. Hence it is safe to eta-reduce `\x. e x` to `e`.
    By contrast, it would be *unsound* to eta-reduce 2 args, `\x y. e x y` to `e`:
    `e 42` diverges when `(\x y. e x y) 42` does not.

 S. It is sound to eta-reduce n arguments in an evaluation context in which all
    calls happen with at least n arguments. (Needs Strictness analysis.)
    NB: This treats evaluations like a call with 0 args.
    NB: This criterion exploits information about how `e` is *used*.

    Example: Given a function `g` like
      `g c = Just (c 1 2 + c 2 3)`
    it is safe to eta-reduce the arg in `g (\x y. e x y)` to `g e` without
    knowing *anything* about `e` (perhaps it's a parameter occ itself), simply
    because `g` always calls its parameter with 2 arguments.
    It is also safe to eta-reduce just one arg, e.g., `g (\x. e x)` to `g e`.
    By contrast, it would *unsound* to eta-reduce 3 args in a call site
    like `g (\x y z. e x y z)` to `g e`, because that diverges when
    `e = \x y. bot`.

    Could we relax to "At least *one call in the same trace* is with n args"?
    Consider what happens for
      ``g2 c = c True `seq` c False 42``
    Here, `g2` will call `c` with 2 two arguments (if there is a call at all).
    But it is unsafe to eta-reduce the arg in `g2 (\x y. e x y)` to `g2 e`
    when `e = \x. if x then bot else id`, because the latter will diverge when
    the former would not.

    See Note [Eta reduction based on evaluation context] for the implementation
    details. This criterion is tested extensively in T21261.

 E. (See fun_arity in tryEtaReduce.) As a perhaps special case on the
    boundary of (A) and (S), when we know that a fun binder `f` is in
    WHNF, we simply assume it has arity 1 and apply (A).  Example:
       g f = f `seq` \x. f x
    Here it's sound eta-reduce `\x. f x` to `f`, because `f` can't be bottom
    after the `seq`. This turned up in #7542.

And here are a few more technical criteria for when it is *not* sound to
eta-reduce that are specific to Core and GHC:

 L. With linear types, eta-reduction can break type-checking:
      f :: A ⊸ B
      g :: A -> B
      g = \x. f x
    The above is correct, but eta-reducing g would yield g=f, the linter will
    complain that g and f don't have the same type. NB: Not unsound in the
    dynamic semantics, but unsound according to the static semantics of Core.

 J. We may not undersaturate join points.
    See Note [Invariants on join points] in GHC.Core, and #20599.

 B. We may not undersaturate functions with no binding.
    See Note [Eta expanding primops].

 W. We may not undersaturate StrictWorkerIds.
    See Note [Strict Worker Ids] in GHC.CoreToStg.Prep.

Here is a list of historic accidents surrounding unsound eta-reduction:

* Consider
        f = \x.f x
        h y = case (case y of { True -> f `seq` True; False -> False }) of
                True -> ...; False -> ...
  If we (unsoundly) eta-reduce f to get f=f, the strictness analyser
  says f=bottom, and replaces the (f `seq` True) with just
  (f `cast` unsafe-co).
  [SG in 2022: I don't think worker/wrapper would do this today.]
  BUT, as things stand, 'f' got arity 1, and it *keeps* arity 1 (perhaps also
  wrongly). So CorePrep eta-expands the definition again, so that it does not
  terminate after all.
  Result: seg-fault because the boolean case actually gets a function value.
  See #1947.

* Never *reduce* arity. For example
      f = \xy. g x y
  Then if h has arity 1 we don't want to eta-reduce because then
  f's arity would decrease, and that is bad
  [SG in 2022: I don't understand this point. There is no `h`, perhaps that
   should have been `g`. Even then, this proposed eta-reduction is invalid by
   criterion (A), which might actually be the point this anecdote is trying to
   make. Perhaps the "no arity decrease" idea is also related to
   Note [Arity robustness]?]

Note [Do not eta reduce PAPs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
I considered eta-reducing if the result is a PAP:
   \x. f e1 e2 x  ==>   f e1 e2

This reduces clutter, sometimes a lot. See Note [Do not eta-expand PAPs]
in GHC.Core.Opt.Simplify.Utils, where we are careful not to eta-expand
a PAP.  If eta-expanding is bad, then eta-reducing is good!

Also the code generator likes eta-reduced PAPs; see GHC.CoreToStg.Prep
Note [No eta reduction needed in rhsToBody].

But note that we don't want to eta-reduce
     \x y.  f <expensive> x y
to
     f <expensive>
The former has arity 2, and repeats <expensive> for every call of the
function; the latter has arity 0, and shares <expensive>.  We don't want
to change behaviour.  Hence the call to exprIsCheap in ok_fun.

I noticed this when examining #18993 and, although it is delicate,
eta-reducing to a PAP happens to fix the regression in #18993.

HOWEVER, if we transform
   \x. f y x   ==>   f y
that might mean that f isn't saturated any more, and does not inline.
This led to some other regressions.

TL;DR currrently we do /not/ eta reduce if the result is a PAP.

Note [Eta reduction with casted arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    (\(x:t3). f (x |> g)) :: t3 -> t2
  where
    f :: t1 -> t2
    g :: t3 ~ t1
This should be eta-reduced to

    f |> (sym g -> t2)

So we need to accumulate a coercion, pushing it inward (past
variable arguments only) thus:
   f (x |> co_arg) |> co  -->  (f |> (sym co_arg -> co)) x
   f (x:t)         |> co  -->  (f |> (t -> co)) x
   f @ a           |> co  -->  (f |> (forall a.co)) @ a
   f @ (g:t1~t2)   |> co  -->  (f |> (t1~t2 => co)) @ (g:t1~t2)
These are the equations for ok_arg.

Note [Eta reduction with casted function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since we are pushing a coercion inwards, it is easy to accommodate
    (\xy. (f x |> g) y)
    (\xy. (f x y) |> g)

See the `(Cast e co)` equation for `go` in `tryEtaReduce`.  The
eta-expander pushes those casts outwards, so you might think we won't
ever see a cast here, but if we have
  \xy. (f x y |> g)
we will call tryEtaReduce [x,y] (f x y |> g), and we'd like that to
work.  This happens in GHC.Core.Opt.Simplify.Utils.mkLam, where
eta-expansion may be turned off (by sm_eta_expand).

Note [Eta reduction based on evaluation context]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Eta reduction soundness], criterion (S) allows us to eta-reduce
`g (\x y. e x y)` to `g e` when we know that `g` always calls its parameter with
at least 2 arguments. So how do we read that off `g`'s demand signature?

Let's take the simple example of #21261, where `g` (actually, `f`) is defined as
  g c = c 1 2 + c 3 4
Then this is how the pieces are put together:

  * Demand analysis infers `<SCS(C1(L))>` for `g`'s demand signature

  * When the Simplifier next simplifies the argument in `g (\x y. e x y)`, it
    looks up the *evaluation context* of the argument in the form of the
    sub-demand `CS(C1(L))` and stores it in the 'SimplCont'.
    (Why does it drop the outer evaluation cardinality of the demand, `S`?
    Because it's irrelevant! When we simplify an expression, we do so under the
    assumption that it is currently under evaluation.)
    This sub-demand literally says "Whenever this expression is evaluated, it
    is also called with two arguments, potentially multiple times".

  * Then the simplifier takes apart the lambda and simplifies the lambda group
    and then calls 'tryEtaReduce' when rebuilding the lambda, passing the
    evaluation context `CS(C1(L))` along. Then we simply peel off 2 call
    sub-demands `Cn` and see whether all of the n's (here: `S=C_1N` and
    `1=C_11`) were strict. And strict they are! Thus, it will eta-reduce
    `\x y. e x y` to `e`.
-}

-- | `tryEtaReduce [x,y,z] e sd` returns `Just e'` if `\x y z -> e` is evaluated
-- according to `sd` and can soundly and gainfully be eta-reduced to `e'`.
-- See Note [Eta reduction soundness]
-- and Note [Eta reduction makes sense] when that is the case.
tryEtaReduce :: [Var] -> CoreExpr -> SubDemand -> Maybe CoreExpr
-- Return an expression equal to (\bndrs. body)
tryEtaReduce bndrs body eval_sd
  = go (reverse bndrs) body (mkRepReflCo (exprType body))
  where
    incoming_arity = count isId bndrs -- See Note [Eta reduction makes sense], point (2)

    go :: [Var]            -- Binders, innermost first, types [a3,a2,a1]
       -> CoreExpr         -- Of type tr
       -> Coercion         -- Of type tr ~ ts
       -> Maybe CoreExpr   -- Of type a1 -> a2 -> a3 -> ts
    -- See Note [Eta reduction with casted arguments]
    -- for why we have an accumulating coercion
    --
    -- Invariant: (go bs body co) returns an expression
    --            equivalent to (\(reverse bs). body |> co)

    -- See Note [Eta reduction with casted function]
    go bs (Cast e co1) co2
      = go bs e (co1 `mkTransCo` co2)

    go bs (Tick t e) co
      | tickishFloatable t
      = fmap (Tick t) $ go bs e co
      -- Float app ticks: \x -> Tick t (e x) ==> Tick t e

    go (b : bs) (App fun arg) co
      | Just (co', ticks) <- ok_arg b arg co (exprType fun)
      = fmap (flip (foldr mkTick) ticks) $ go bs fun co'
            -- Float arg ticks: \x -> e (Tick t x) ==> Tick t e

    go remaining_bndrs fun co
      | all isTyVar remaining_bndrs
            -- If all the remaining_bnrs are tyvars, then the etad_exp
            --    will be trivial, which is what we want.
            -- e.g. We might have  /\a \b. f [a] b, and we want to
            --      eta-reduce to  /\a. f [a]
            -- We don't want to give up on this one: see #20040
            -- See Note [Eta reduction makes sense], point (1)
      , remaining_bndrs `ltLength` bndrs
            -- Only reply Just if /something/ has happened
      , ok_fun fun
      , let etad_expr = mkLams (reverse remaining_bndrs) (mkCast fun co)
            used_vars = exprFreeVars etad_expr
            reduced_bndrs = mkVarSet (dropList remaining_bndrs bndrs)
      , used_vars `disjointVarSet` reduced_bndrs
          -- Check for any of the binders free in the result,
          -- including the accumulated coercion
          -- See Note [Eta reduction makes sense], intro and point (1)
      = Just etad_expr

    go _remaining_bndrs _fun  _  = -- pprTrace "tER fail" (ppr _fun $$ ppr _remaining_bndrs) $
                                   Nothing

    ---------------
    -- See Note [Eta reduction makes sense], point (1)
    ok_fun (App fun (Type {})) = ok_fun fun
    ok_fun (Cast fun _)        = ok_fun fun
    ok_fun (Tick _ expr)       = ok_fun expr
    ok_fun (Var fun_id)        = is_eta_reduction_sound fun_id || all ok_lam bndrs
    ok_fun _fun                = False

    ---------------
    -- See Note [Eta reduction soundness], this is THE place to check soundness!
    is_eta_reduction_sound fun =
      -- Check that eta-reduction won't make the program stricter...
      (fun_arity fun >= incoming_arity            -- criterion (A) and (E)
        || all_calls_with_arity incoming_arity)   -- criterion (S)
      -- ... and that the function can be eta reduced to arity 0
      -- without violating invariants of Core and GHC
      && canEtaReduceToArity fun 0 0              -- criteria (L), (J), (W), (B)
    all_calls_with_arity n = isStrict (peelManyCalls n eval_sd)
       -- See Note [Eta reduction based on evaluation context]

    ---------------
    fun_arity fun
       | isLocalId fun
       , isStrongLoopBreaker (idOccInfo fun) = 0
           -- See Note [Eta reduction makes sense], point (3)
       | arity > 0                           = arity
       | isEvaldUnfolding (idUnfolding fun)  = 1
           -- See Note [Eta reduction soundness], criterion (E)
       | otherwise                           = 0
       where
         arity = idArity fun

    ---------------
    ok_lam v = isTyVar v || isEvVar v
    -- See Note [Eta reduction makes sense], point (2)

    ---------------
    ok_arg :: Var              -- Of type bndr_t
           -> CoreExpr         -- Of type arg_t
           -> Coercion         -- Of kind (t1~t2)
           -> Type             -- Type (arg_t -> t1) of the function
                               --      to which the argument is supplied
           -> Maybe (Coercion  -- Of type (arg_t -> t1 ~  bndr_t -> t2)
                               --   (and similarly for tyvars, coercion args)
                    , [CoreTickish])
    -- See Note [Eta reduction with casted arguments]
    ok_arg bndr (Type ty) co _
       | Just tv <- getTyVar_maybe ty
       , bndr == tv  = Just (mkHomoForAllCos [tv] co, [])
    ok_arg bndr (Var v) co fun_ty
       | bndr == v
       , let mult = idMult bndr
       , Just (fun_mult, _, _) <- splitFunTy_maybe fun_ty
       , mult `eqType` fun_mult -- There is no change in multiplicity, otherwise we must abort
       = Just (mkFunResCo Representational (idScaledType bndr) co, [])
    ok_arg bndr (Cast e co_arg) co fun_ty
       | (ticks, Var v) <- stripTicksTop tickishFloatable e
       , Just (fun_mult, _, _) <- splitFunTy_maybe fun_ty
       , bndr == v
       , fun_mult `eqType` idMult bndr
       = Just (mkFunCo Representational (multToCo fun_mult) (mkSymCo co_arg) co, ticks)
       -- The simplifier combines multiple casts into one,
       -- so we can have a simple-minded pattern match here
    ok_arg bndr (Tick t arg) co fun_ty
       | tickishFloatable t, Just (co', ticks) <- ok_arg bndr arg co fun_ty
       = Just (co', t:ticks)

    ok_arg _ _ _ _ = Nothing

-- | Can we eta-reduce the given function to the specified arity?
-- See Note [Eta reduction soundness], criteria (B), (J), (W) and (L).
canEtaReduceToArity :: Id -> JoinArity -> Arity -> Bool
canEtaReduceToArity fun dest_join_arity dest_arity =
  not $
        hasNoBinding fun -- (B)
       -- Don't undersaturate functions with no binding.

    ||  ( isJoinId fun && dest_join_arity < idJoinArity fun ) -- (J)
       -- Don't undersaturate join points.
       -- See Note [Invariants on join points] in GHC.Core, and #20599

    || ( dest_arity < idCbvMarkArity fun ) -- (W)
       -- Don't undersaturate StrictWorkerIds.
       -- See Note [Strict Worker Ids] in GHC.CoreToStg.Prep.

    ||  isLinearType (idType fun) -- (L)
       -- Don't perform eta reduction on linear types.
       -- If `f :: A %1-> B` and `g :: A -> B`,
       -- then `g x = f x` is OK but `g = f` is not.


{- *********************************************************************
*                                                                      *
              The "push rules"
*                                                                      *
************************************************************************

Here we implement the "push rules" from FC papers:

* The push-argument rules, where we can move a coercion past an argument.
  We have
      (fun |> co) arg
  and we want to transform it to
    (fun arg') |> co'
  for some suitable co' and transformed arg'.

* The PushK rule for data constructors.  We have
       (K e1 .. en) |> co
  and we want to transform to
       (K e1' .. en')
  by pushing the coercion into the arguments
-}

pushCoArgs :: CoercionR -> [CoreArg] -> Maybe ([CoreArg], MCoercion)
pushCoArgs co []         = return ([], MCo co)
pushCoArgs co (arg:args) = do { (arg',  m_co1) <- pushCoArg  co  arg
                              ; case m_co1 of
                                  MCo co1 -> do { (args', m_co2) <- pushCoArgs co1 args
                                                 ; return (arg':args', m_co2) }
                                  MRefl  -> return (arg':args, MRefl) }

pushMCoArg :: MCoercionR -> CoreArg -> Maybe (CoreArg, MCoercion)
pushMCoArg MRefl    arg = Just (arg, MRefl)
pushMCoArg (MCo co) arg = pushCoArg co arg

pushCoArg :: CoercionR -> CoreArg -> Maybe (CoreArg, MCoercion)
-- We have (fun |> co) arg, and we want to transform it to
--         (fun arg) |> co
-- This may fail, e.g. if (fun :: N) where N is a newtype
-- C.f. simplCast in GHC.Core.Opt.Simplify
-- 'co' is always Representational
pushCoArg co arg
  | Type ty <- arg
  = do { (ty', m_co') <- pushCoTyArg co ty
       ; return (Type ty', m_co') }
  | otherwise
  = do { (arg_mco, m_co') <- pushCoValArg co
       ; let arg_mco' = checkReflexiveMCo arg_mco
             -- checkReflexiveMCo: see Note [Check for reflexive casts in eta expansion]
             -- The coercion is very often (arg_co -> res_co), but without
             -- the argument coercion actually being ReflCo
       ; return (arg `mkCastMCo` arg_mco', m_co') }

pushCoTyArg :: CoercionR -> Type -> Maybe (Type, MCoercionR)
-- We have (fun |> co) @ty
-- Push the coercion through to return
--         (fun @ty') |> co'
-- 'co' is always Representational
-- If the returned coercion is Nothing, then it would have been reflexive;
-- it's faster not to compute it, though.
pushCoTyArg co ty
  -- The following is inefficient - don't do `eqType` here, the coercion
  -- optimizer will take care of it. See #14737.
  -- -- | tyL `eqType` tyR
  -- -- = Just (ty, Nothing)

  | isReflCo co
  = Just (ty, MRefl)

  | isForAllTy_ty tyL
  = assertPpr (isForAllTy_ty tyR) (ppr co $$ ppr ty) $
    Just (ty `mkCastTy` co1, MCo co2)

  | otherwise
  = Nothing
  where
    Pair tyL tyR = coercionKind co
       -- co :: tyL ~ tyR
       -- tyL = forall (a1 :: k1). ty1
       -- tyR = forall (a2 :: k2). ty2

    co1 = mkSymCo (mkNthCo Nominal 0 co)
       -- co1 :: k2 ~N k1
       -- Note that NthCo can extract a Nominal equality between the
       -- kinds of the types related by a coercion between forall-types.
       -- See the NthCo case in GHC.Core.Lint.

    co2 = mkInstCo co (mkGReflLeftCo Nominal ty co1)
        -- co2 :: ty1[ (ty|>co1)/a1 ] ~ ty2[ ty/a2 ]
        -- Arg of mkInstCo is always nominal, hence mkNomReflCo

pushCoValArg :: CoercionR -> Maybe (MCoercionR, MCoercionR)
-- We have (fun |> co) arg
-- Push the coercion through to return
--         (fun (arg |> co_arg)) |> co_res
-- 'co' is always Representational
-- If the second returned Coercion is actually Nothing, then no cast is necessary;
-- the returned coercion would have been reflexive.
pushCoValArg co
  -- The following is inefficient - don't do `eqType` here, the coercion
  -- optimizer will take care of it. See #14737.
  -- -- | tyL `eqType` tyR
  -- -- = Just (mkRepReflCo arg, Nothing)

  | isReflCo co
  = Just (MRefl, MRefl)

  | isFunTy tyL
  , (co_mult, co1, co2) <- decomposeFunCo Representational co
  , isReflexiveCo co_mult
    -- We can't push the coercion in the case where co_mult isn't reflexivity:
    -- it could be an unsafe axiom, and losing this information could yield
    -- ill-typed terms. For instance (fun x ::(1) Int -> (fun _ -> () |> co) x)
    -- with co :: (Int -> ()) ~ (Int %1 -> ()), would reduce to (fun x ::(1) Int
    -- -> (fun _ ::(Many) Int -> ()) x) which is ill-typed

              -- If   co  :: (tyL1 -> tyL2) ~ (tyR1 -> tyR2)
              -- then co1 :: tyL1 ~ tyR1
              --      co2 :: tyL2 ~ tyR2
  = assertPpr (isFunTy tyR) (ppr co $$ ppr arg) $
    Just (coToMCo (mkSymCo co1), coToMCo co2)
    -- Critically, coToMCo to checks for ReflCo; the whole coercion may not
    -- be reflexive, but either of its components might be
    -- We could use isReflexiveCo, but it's not clear if the benefit
    -- is worth the cost, and it makes no difference in #18223

  | otherwise
  = Nothing
  where
    arg = funArgTy tyR
    Pair tyL tyR = coercionKind co

pushCoercionIntoLambda
    :: HasDebugCallStack => InScopeSet -> Var -> CoreExpr -> CoercionR -> Maybe (Var, CoreExpr)
-- This implements the Push rule from the paper on coercions
--    (\x. e) |> co
-- ===>
--    (\x'. e |> co')
pushCoercionIntoLambda in_scope x e co
    | assert (not (isTyVar x) && not (isCoVar x)) True
    , Pair s1s2 t1t2 <- coercionKind co
    , Just (_, _s1,_s2) <- splitFunTy_maybe s1s2
    , Just (w1, t1,_t2) <- splitFunTy_maybe t1t2
    , (co_mult, co1, co2) <- decomposeFunCo Representational co
    , isReflexiveCo co_mult
      -- We can't push the coercion in the case where co_mult isn't
      -- reflexivity. See pushCoValArg for more details.
    = let
          -- Should we optimize the coercions here?
          -- Otherwise they might not match too well
          x' = x `setIdType` t1 `setIdMult` w1
          in_scope' = in_scope `extendInScopeSet` x'
          subst = extendIdSubst (mkEmptySubst in_scope')
                                x
                                (mkCast (Var x') (mkSymCo co1))
            -- We substitute x' for x, except we need to preserve types.
            -- The types are as follows:
            --   x :: s1,  x' :: t1,  co1 :: s1 ~# t1,
            -- so we extend the substitution with x |-> (x' |> sym co1).
      in Just (x', substExpr subst e `mkCast` co2)
    | otherwise
    = pprTrace "exprIsLambda_maybe: Unexpected lambda in case" (ppr (Lam x e))
      Nothing

pushCoDataCon :: DataCon -> [CoreExpr] -> Coercion
              -> Maybe (DataCon
                       , [Type]      -- Universal type args
                       , [CoreExpr]) -- All other args incl existentials
-- Implement the KPush reduction rule as described in "Down with kinds"
-- The transformation applies iff we have
--      (C e1 ... en) `cast` co
-- where co :: (T t1 .. tn) ~ to_ty
-- The left-hand one must be a T, because exprIsConApp returned True
-- but the right-hand one might not be.  (Though it usually will.)
pushCoDataCon dc dc_args co
  | isReflCo co || from_ty `eqType` to_ty  -- try cheap test first
  , let (univ_ty_args, rest_args) = splitAtList (dataConUnivTyVars dc) dc_args
  = Just (dc, map exprToType univ_ty_args, rest_args)

  | Just (to_tc, to_tc_arg_tys) <- splitTyConApp_maybe to_ty
  , to_tc == dataConTyCon dc
        -- These two tests can fail; we might see
        --      (C x y) `cast` (g :: T a ~ S [a]),
        -- where S is a type function.  In fact, exprIsConApp
        -- will probably not be called in such circumstances,
        -- but there's nothing wrong with it

  = let
        tc_arity       = tyConArity to_tc
        dc_univ_tyvars = dataConUnivTyVars dc
        dc_ex_tcvars   = dataConExTyCoVars dc
        arg_tys        = dataConRepArgTys dc

        non_univ_args  = dropList dc_univ_tyvars dc_args
        (ex_args, val_args) = splitAtList dc_ex_tcvars non_univ_args

        -- Make the "Psi" from the paper
        omegas = decomposeCo tc_arity co (tyConRolesRepresentational to_tc)
        (psi_subst, to_ex_arg_tys)
          = liftCoSubstWithEx Representational
                              dc_univ_tyvars
                              omegas
                              dc_ex_tcvars
                              (map exprToType ex_args)

          -- Cast the value arguments (which include dictionaries)
        new_val_args = zipWith cast_arg (map scaledThing arg_tys) val_args
        cast_arg arg_ty arg = mkCast arg (psi_subst arg_ty)

        to_ex_args = map Type to_ex_arg_tys

        dump_doc = vcat [ppr dc,      ppr dc_univ_tyvars, ppr dc_ex_tcvars,
                         ppr arg_tys, ppr dc_args,
                         ppr ex_args, ppr val_args, ppr co, ppr from_ty, ppr to_ty, ppr to_tc
                         , ppr $ mkTyConApp to_tc (map exprToType $ takeList dc_univ_tyvars dc_args) ]
    in
    assertPpr (eqType from_ty (mkTyConApp to_tc (map exprToType $ takeList dc_univ_tyvars dc_args))) dump_doc $
    assertPpr (equalLength val_args arg_tys) dump_doc $
    Just (dc, to_tc_arg_tys, to_ex_args ++ new_val_args)

  | otherwise
  = Nothing

  where
    Pair from_ty to_ty = coercionKind co

collectBindersPushingCo :: CoreExpr -> ([Var], CoreExpr)
-- Collect lambda binders, pushing coercions inside if possible
-- E.g.   (\x.e) |> g         g :: <Int> -> blah
--        = (\x. e |> Nth 1 g)
--
-- That is,
--
-- collectBindersPushingCo ((\x.e) |> g) === ([x], e |> Nth 1 g)
collectBindersPushingCo e
  = go [] e
  where
    -- Peel off lambdas until we hit a cast.
    go :: [Var] -> CoreExpr -> ([Var], CoreExpr)
    -- The accumulator is in reverse order
    go bs (Lam b e)   = go (b:bs) e
    go bs (Cast e co) = go_c bs e co
    go bs e           = (reverse bs, e)

    -- We are in a cast; peel off casts until we hit a lambda.
    go_c :: [Var] -> CoreExpr -> CoercionR -> ([Var], CoreExpr)
    -- (go_c bs e c) is same as (go bs e (e |> c))
    go_c bs (Cast e co1) co2 = go_c bs e (co1 `mkTransCo` co2)
    go_c bs (Lam b e)    co  = go_lam bs b e co
    go_c bs e            co  = (reverse bs, mkCast e co)

    -- We are in a lambda under a cast; peel off lambdas and build a
    -- new coercion for the body.
    go_lam :: [Var] -> Var -> CoreExpr -> CoercionR -> ([Var], CoreExpr)
    -- (go_lam bs b e c) is same as (go_c bs (\b.e) c)
    go_lam bs b e co
      | isTyVar b
      , let Pair tyL tyR = coercionKind co
      , assert (isForAllTy_ty tyL) $
        isForAllTy_ty tyR
      , isReflCo (mkNthCo Nominal 0 co)  -- See Note [collectBindersPushingCo]
      = go_c (b:bs) e (mkInstCo co (mkNomReflCo (mkTyVarTy b)))

      | isCoVar b
      , let Pair tyL tyR = coercionKind co
      , assert (isForAllTy_co tyL) $
        isForAllTy_co tyR
      , isReflCo (mkNthCo Nominal 0 co)  -- See Note [collectBindersPushingCo]
      , let cov = mkCoVarCo b
      = go_c (b:bs) e (mkInstCo co (mkNomReflCo (mkCoercionTy cov)))

      | isId b
      , let Pair tyL tyR = coercionKind co
      , assert (isFunTy tyL) $ isFunTy tyR
      , (co_mult, co_arg, co_res) <- decomposeFunCo Representational co
      , isReflCo co_mult -- See Note [collectBindersPushingCo]
      , isReflCo co_arg  -- See Note [collectBindersPushingCo]
      = go_c (b:bs) e co_res

      | otherwise = (reverse bs, mkCast (Lam b e) co)

{-

Note [collectBindersPushingCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We just look for coercions of form
   <type> % w -> blah
(and similarly for foralls) to keep this function simple.  We could do
more elaborate stuff, but it'd involve substitution etc.

-}

{- *********************************************************************
*                                                                      *
                Join points
*                                                                      *
********************************************************************* -}

-------------------
-- | Split an expression into the given number of binders and a body,
-- eta-expanding if necessary. Counts value *and* type binders.
etaExpandToJoinPoint :: JoinArity -> CoreExpr -> ([CoreBndr], CoreExpr)
etaExpandToJoinPoint join_arity expr
  = go join_arity [] expr
  where
    go 0 rev_bs e         = (reverse rev_bs, e)
    go n rev_bs (Lam b e) = go (n-1) (b : rev_bs) e
    go n rev_bs e         = case etaBodyForJoinPoint n e of
                              (bs, e') -> (reverse rev_bs ++ bs, e')

etaExpandToJoinPointRule :: JoinArity -> CoreRule -> CoreRule
etaExpandToJoinPointRule _ rule@(BuiltinRule {})
  = warnPprTrace True "Can't eta-expand built-in rule:" (ppr rule)
      -- How did a local binding get a built-in rule anyway? Probably a plugin.
    rule
etaExpandToJoinPointRule join_arity rule@(Rule { ru_bndrs = bndrs, ru_rhs = rhs
                                               , ru_args  = args })
  | need_args == 0
  = rule
  | need_args < 0
  = pprPanic "etaExpandToJoinPointRule" (ppr join_arity $$ ppr rule)
  | otherwise
  = rule { ru_bndrs = bndrs ++ new_bndrs, ru_args = args ++ new_args
         , ru_rhs = new_rhs }
  where
    need_args = join_arity - length args
    (new_bndrs, new_rhs) = etaBodyForJoinPoint need_args rhs
    new_args = varsToCoreExprs new_bndrs

-- Adds as many binders as asked for; assumes expr is not a lambda
etaBodyForJoinPoint :: Int -> CoreExpr -> ([CoreBndr], CoreExpr)
etaBodyForJoinPoint need_args body
  = go need_args (exprType body) (init_subst body) [] body
  where
    go 0 _  _     rev_bs e
      = (reverse rev_bs, e)
    go n ty subst rev_bs e
      | Just (tv, res_ty) <- splitForAllTyCoVar_maybe ty
      , let (subst', tv') = substVarBndr subst tv
      = go (n-1) res_ty subst' (tv' : rev_bs) (e `App` varToCoreExpr tv')
      | Just (mult, arg_ty, res_ty) <- splitFunTy_maybe ty
      , let (subst', b) = freshEtaId n subst (Scaled mult arg_ty)
      = go (n-1) res_ty subst' (b : rev_bs) (e `App` Var b)
      | otherwise
      = pprPanic "etaBodyForJoinPoint" $ int need_args $$
                                         ppr body $$ ppr (exprType body)

    init_subst e = mkEmptyTCvSubst (mkInScopeSet (exprFreeVars e))



--------------
freshEtaId :: Int -> TCvSubst -> Scaled Type -> (TCvSubst, Id)
-- Make a fresh Id, with specified type (after applying substitution)
-- It should be "fresh" in the sense that it's not in the in-scope set
-- of the TvSubstEnv; and it should itself then be added to the in-scope
-- set of the TvSubstEnv
--
-- The Int is just a reasonable starting point for generating a unique;
-- it does not necessarily have to be unique itself.
freshEtaId n subst ty
      = (subst', eta_id')
      where
        Scaled mult' ty' = Type.substScaledTyUnchecked subst ty
        eta_id' = uniqAway (getTCvInScope subst) $
                  mkSysLocalOrCoVar (fsLit "eta") (mkBuiltinUnique n) mult' ty'
                  -- "OrCoVar" since this can be used to eta-expand
                  -- coercion abstractions
        subst'  = extendTCvInScope subst eta_id'
