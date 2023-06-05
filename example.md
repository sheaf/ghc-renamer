<!--
Compile the HTML slides with Pandoc:

pandoc -t revealjs -s -o example.html example.md -V revealjs-url=reveal.js -V theme=welltyped_theme -V controlsTutorial=false -V controlsLayout=edges -V slideNumber='c/t' -V transition=slide

Custom theme in reveal.js/dist/themes/welltyped_theme.css
-->

---
author: Sam Derbyshire, Well-Typed
title: GHC's renamer (Part II)
subtitle: GHC Contributors' Workshop
date: June 7th, 2023
---

## The problem

```haskell
{-# COMPLETE U #-}
pattern U :: ()
pattern U = ()

foo :: Identity Char
foo = do
  U <- return ()
  return 'c'
```

```markdown
error:
    * No instance for `MonadFail Identity'
        arising from a do statement
        with the failable pattern `U'
    * In a stmt of a 'do' block: U <- return ()
```

<p class="indicator">⭲</p>

## Context

This has been reported many times: [#15681](https://gitlab.haskell.org/ghc/ghc/-/issues/15681) [#16470](https://gitlab.haskell.org/ghc/ghc/-/issues/16470) [#22004](https://gitlab.haskell.org/ghc/ghc/-/issues/22004) [#23458](https://gitlab.haskell.org/ghc/ghc/-/issues/23458) (related: [#16618](https://gitlab.haskell.org/ghc/ghc/-/issues/16618)).

Fundamental problem:

  - the renamer decides how to elaborate a
  - the pattern-match checker runs after type-checking (it needs types)

:::{.element: class="fragment"}
However, in this situation, we don't need types to be able to see that the
pattern match succeeds (this is different from situation which involve GADT
pattern matches).

<p class="indicator">⭲</p>
:::

## Investigation

From the error message, we can see that we emit a `MonadFail Identity`
Wanted constraint when typechecking `foo`.

Where? We can search for `MonadFail` to find out!

<p class="indicator">⭲</p>

## In the renamer

`GHC.Rename.Expr`:

```haskell
rnStmt ctxt rnBody (L loc (BindStmt _ pat (L lb body))) thing_inside
  = do  { -- ...
        ; (fail_op, fvs2) <- monadFailOp pat ctxt
        ; -- ...
        }
```

```haskell
monadFailOp :: LPat GhcPs
            -> HsStmtContext GhcRn
            -> RnM (FailOperator GhcRn, FreeVars)
monadFailOp pat ctxt = do
    dflags <- getDynFlags
    if | isIrrefutableHsPat dflags pat -> return (Nothing, emptyFVs)
       | not (isMonadStmtContext ctxt) -> return (Nothing, emptyFVs)
       | otherwise -> getMonadFailOp ctxt
```

<p class="indicator">⭲</p>

## In the typechecker
`GHC.Tc.Gen.Match` (`Gen` = constraint generator):

```haskell
tcDoStmt ctxt (BindStmt xbsrn pat rhs) res_ty thing_inside
  = do  { -- ...
        ; fail_op' <- fmap join . forM (xbsrn_failOp xbsrn) $ \fail ->
            tcMonadFailOp (DoPatOrigin pat) pat' fail new_res_ty
        ; -- ...
        ; return (BindStmt xbstc pat' rhs', thing) }
```

```haskell
tcMonadFailOp orig pat fail_op res_ty = do
    dflags <- getDynFlags
    if isIrrefutableHsPat dflags pat
    then return Nothing
    else Just . snd <$> (tcSyntaxOp orig fail_op [synKnownType stringTy]
                          (mkCheckExpType res_ty) $ \_ _ -> return ())
```

:::{.element: class="fragment"}
`tcSyntaxOp` is going to instantiate `fail :: MonadFail m => String -> m a`,
which will emit a `MonadFail Identity` Wanted constraint. <p class="indicator">⭲</p>
:::

## Irrefutable patterns

:::{.element: class="fragment"}
```haskell
isIrrefutableHsPat :: Bool -- ^ Is @-XStrict@ enabled?
                   -> Pat (GhcPass p) -> Bool
isIrrefutableHsPat is_strict = \case
  WildPat {} -> True
  VarPat {}  -> True
  -- ...
  ConPat
    { pat_con  = con
    , pat_args = details } ->
      case ghcPass @p of
        GhcPs -> False -- Conservative
        GhcRn -> False -- Conservative
        GhcTc -> case con of
          L _ (PatSynCon _pat)  -> False -- Conservative
          L _ (RealDataCon con) ->
            isJust (tyConSingleDataCon_maybe (dataConTyCon con))
            && all goL (hsConPatArgs details)
```

<p class="indicator">⭲</p>
:::

## Approach

Let's fix `isIrrefutableHsPat` for `ConPat` at `GhcRn` and `GhcTc` stages.

```haskell
data Pat p
  = -- ...
  | ConPat {
        pat_con_ext :: XConPat p,
        pat_con     :: XRec p (ConLikeP p),
        pat_args    :: HsConPatDetails p
    }
```

:::{.element: class="fragment"}
```haskell
type instance ConLikeP GhcPs = RdrName
type instance ConLikeP GhcRn = Name
type instance ConLikeP GhcTc = ConLike

type instance XConPat GhcPs = EpAnn [AddEpAnn]
type instance XConPat GhcRn = NoExtField
type instance XConPat GhcTc = ConPatTc
```

<p class="indicator">⭲</p>
:::

## What to change?

We need two things:

  - for a `DataCon`, whether it is irrefutable (`tyConSingleDataCon_maybe`);
  - for a `PatSyn`, whether it is the unique member of a COMPLETE set.

For `PatSyn`s, we have the choice of storing whether the `PatSyn` is
irrefutable in the `PatSyn` itself, or of threading through the `COMPLETE`
pragmas to `isIrrefutableHsPat`.

<br/>

:::{.element: class="fragment"}
I've chosen to thread through `COMPLETE` pragmas, as that would also allow
us to handle `or` patterns:

```haskell
{-# COMPLETE P, Q #-}
do (one of {P; Q}) <- ...
```

<p class="indicator">⭲</p>
:::

## Using the extension field

Idea: let's store whether the constructor pattern is a `DataCon` or a `PatSyn`,
and if it's a `DataCon` whether it's irrefutable, in `XConPat GhcRn`.  

```haskell
data ConInfoType
  = ConIsData { conIsSingleDataCon :: Bool }
  | ConIsPatSyn
```

(Alternatively we could change `ConLikeP GhcRn` to `ConLikeName`, but that
wouldn't handle the `tyConSingleDataCon` situation.)

<br/>

:::{.element: class="fragment"}
... we still need to take into account COMPLETE pragmas. <p class="indicator">⭲</p>
:::

## Information flow

We now have to audit the calls to `isIrrefutableHsPat`; the HLS call hierarchy
functionality is very useful for that.

<ol type="1" class="fragment">

  <li>
    In the renamer:
    <ol type="a">
      <li>
        `stmtTreeToStmts .. (StmtTreeApplicative ..)`
      </li>
      <li>
        `monadFailOp` in `rnStmt .. (BindStmt ..)` and `rn_rec_stmt .. (BindStmt ..)`
      </li>
    </ol>
  </li>

  <li>
    In the typechecker
    <ol type="a">
      <li>
        When creating a `PatSyn` in `tcPatSynMatcher` (from `tcPatSynDecl`).
      </li>
      <li>
        Calls to `tcMonadFailOp` in `tcApplicativeStmts`, `tcDoStmt`, `tcMcStmt`.
      </li>
    </ol>
  </li>

</ol>

:::{.element: class="fragment"}
This means that we are going to need `COMPLETE` pattern information in the
renamer, and before we have actually typechecked `PatSyn`s in the typechecker.
<p class="indicator">⭲</p>
:::

## COMPLETE pragmas

First, we should see how how `COMPLETE` pragmas are represented in the compiler.

:::{.element: class="fragment"}
```haskell
-- In Language.Haskell.Syntax.Binds
data Sig pass
  = -- ...
  | CompleteMatchSig
    (XCompleteMatchSig pass) -- (NB: only exact-print info here)
    (XRec pass [LIdP pass])
    (Maybe (LIdP pass))
```

```haskell
-- In GHC.Types.CompleteMatch
data CompleteMatch = CompleteMatch
  { cmConLikes :: UniqDSet ConLike, cmResultTyCon :: Maybe TyCon }

data TcGblEnv
  = TcGblEnv
  { -- ...
  , tcg_complete_matches :: !CompleteMatches }
```
:::


## Threading it through

Let's add a field to the typechecker environment (used for both the typechecker
and the renamer):

```haskell
data TcGblEnv
  = TcGblEnv { -- ...
             , tcg_complete_matches :: !CompleteMatches
   {- NEW -} , tcg_complete_matches_rn :: !CompleteMatchesRn
             , -- ...
             }

-- NEW
type CompleteMatchesRn = [CompleteMatchRn]
data CompleteMatchRn = CompleteMatchRn
  { cmConLikesRn :: NameSet -- ^ The set of `ConLike` values
  }
```

<p class="indicator">⭲</p>

## Populating the field

Let's see then how to thread through the correct information; we need to
ensure that we first rename pattern synonym COMPLETE signatures and add that
information to the `TcGblEnv` environment before we call `isIrrefutableHsPat`.

<p class="indicator">⭲</p>

## Call hierarchy of `monadFailOp`

Let's use HLS to find the call hierarchy of `GHC.Rename.Expr.monadFailOp`.

:::{.element: class="fragment"}
  - `monadFailOp`
  - `rnStmt`
  - `rnStmtsWithFreeVars`
  - `rnExpr`
  - `rnGRHS'`
  - `rnGHRS`
  - `rnGHRSs`
  - `rnBind`
  - `rnLBind`
  - `rnValBindsRHS`
  - `rnSrcDecls` <p class="indicator">⭲</p>
:::

## Focus on `rnValBindsRHS`

`rnValBindsRHS` is interesting, as this is where we also rename signatures:

```haskell
rnValBindsRHS :: HsSigCtxt
              -> HsValBindsLR GhcRn GhcPs
              -> RnM (HsValBinds GhcRn, DefUses)
rnValBindsRHS ctxt (ValBinds _ mbinds sigs)
  = do { (sigs', sig_fvs) <- renameSigs ctxt sigs
       ; binds_w_dus <- mapBagM (rnLBind (mkScopedTvFn sigs')) mbinds
       -- ...
       }
```
:::{.element: class="fragment"}
Here we clearly see that we rename `Sig`s first (which includes `COMPLETE` pragmas),
and then rename value bindings. So we can simply extend the `TcGblEnv`, e.g.:

```haskell
do { (sigs', sig_fvs) <- renameSigs ctxt sigs
   ; binds_w_dus <-
      updGblEnv (add_complete_sigs_rn sigs') $
      mapBagM (rnLBind (mkScopedTvFn sigs')) mbinds
   -- ...
   }
```
:::

## Typechecker

Now, this does the trick in the renamer. However, we also need to make sure
to persist the information in `tcg_complete_matches`.
So we should return the `CompleteMatchesRn` at the end of `rnValBindsRHS`,
and make sure to add them to the final typechecker environment at the end of
`rnSrcDecls`.

<p class="indicator">⭲</p>

## Using `tcg_complete_matches`

We then need to look this information up in `isIrrefutableHsPat`.

```haskell
isIrrefutableHsPat :: IsPass p => LPat (GhcPass p) -> TcM Bool
isIrrefutableHsPat pat
  = do { strict <- xoptM LangExt.Strict
       ; comps <- get_complete_matches_rn
       ; return $ is_irrefutable_pat comps strict pat }
```

:::{.element: class="fragment"}
However, we need to make sure we include **all** complete matches, not just
those from the local module.  

Let's find how GHC already does that, for `tcg_complete_matches`.

<p class="indicator">⭲</p>
:::

## Taking inspiration

Inspired by `mkDsEnvsFromTcGbl` and `initDsWithModGuts` in `GHC.HsToCore.Monad`:

```haskell
get_complete_matches_rn :: TcM CompleteMatchesRn
get_complete_matches_rn
  = do { hsc_env <- getTopEnv
       ; tcg_env <- getGblEnv
       ; eps <- liftIO $ hscEPS hsc_env
       ; return $
          completeMatchesRn
            (   hptCompleteSigs hsc_env      -- from the home package
             ++ eps_complete_matches eps )   -- from imports
          ++ tcg_complete_matches_rn tcg_env -- from the current module
       }
```

<p class="indicator">⭲</p>

## Updating `isIrrefutableHsPat`

In the worker function `is_irrefutable_pat` we can then make use of this
information, in the `ConPat` case for a `PatSyn`. We want to check whether
the `PatSyn` `Name` is the single member of one of the `COMPLETE` sets.

:::{.element: class="fragment"}
```haskell
is_irrefutable_hs_pat complete_matches strict_enabled = \case
  -- ...
  ConPat
    { pat_con_ext = ext
    , pat_con     = con
    , pat_args    = details } ->
      con_irref && all go (hsConPatArgs details)
        where
          con_irref = case ghcPass @p of
            GhcPs -> False -- Conservative
            GhcRn -> case ext of
              ConIsData { conIsSingleDataCon = irref } -> irref
              ConIsPatSyn -> any (single_match con) complete_matches
            GhcTc -> -- ...
```

<p class="indicator">⭲</p>
:::

## Outcome

Now that everything is in place, we try some tests programs, but we run into:

```markdown
panic! (the 'impossible' happened)
  GHC version 9.7.20230607:
        missing fail op
  Pattern match: U is failable, and fail_expr was left unset
```

:::{.element: class="fragment"}
Work backwards: search for "missing fail op".

<p class="indicator">⭲</p>
:::

## `dsHandleMonadicFailure`

```haskell
dsHandleMonadicFailure ctx pat match m_fail_op =
  case shareFailureHandler match of
    MR_Infallible body -> body
    MR_Fallible body ->
      case m_fail_op of
-- Note that (non-monadic) list comprehension, pattern guards, etc could
-- have fallible bindings without an explicit failure op, but this is
-- handled elsewhere. See Note [Failing pattern matches in Stmts] the
-- breakdown of regular and special binds.
        Nothing -> pprPanic "missing fail op" $
          text "Pattern match:" <+> ppr pat <+>
          text "is failable, and fail_expr was left unset"
        Just fail_op -> -- ...
```

:::{.element: class="fragment"}
Where are we constructing `MR_Fallible`? <p class="indicator">⭲</p>
:::

## `mkCoSynCaseMatchResult`
`mkCoSynCaseMatchResult` called in `matchPatSyn` looks relevant!

```haskell
mkCoSynCaseMatchResult :: Id -> Type -> CaseAlt PatSyn -> MatchResult CoreExpr
mkCoSynCaseMatchResult var ty alt = MR_Fallible $ mkPatSynCase var ty alt
```

:::{.element: class="fragment"}
Problem: the pattern synonym matcher always returns a fallible match
result. This is actually correct in general, e.g.:

```haskell
pattern Bogus :: Int
pattern Bogus = 3
{-# COMPLETE Bogus #-}

bogus :: Identity Bool
bogus = do
  Bogus <- return 4
  return False
```

<p class="indicator">⭲</p>
:::

## Handling monadic failure

We should accept that previous program, but crash at runtime,
using a failure operator that raises a pattern match failure exception:

```haskell
dsHandleMonadicFailure ctx pat match m_fail_op body_ty =
  case shareFailureHandler match of
    MR_Infallible body -> body
    MR_Fallible body -> do
      case m_fail_op of
        Nothing ->
          do error_expr <- mkErrorAppDs pAT_ERROR_ID body_ty (ppr pat)
-- NB: a custom error would be better than re-using pAT_ERROR_ID
             body error_expr
        Just fail_op -> -- ...
```

Note that we need to know the overall return type of the `do` expression to
know the return type of the error; so we modify `dsHandleMonadicFailure`
to get that extra type (straightforward).

## Finishing up

- Adding tests.
- Writing Notes.

## Other tickets

- Simple error message improvements:
  - Error messages for instances of a non-class [#22688](https://gitlab.haskell.org/ghc/ghc/-/issues/22688)
    and [#23462](https://gitlab.haskell.org/ghc/ghc/-/issues/23462).
  - Improve import suggestions [#20771](https://gitlab.haskell.org/ghc/ghc/-/issues/20771).

- Dependency order with view patterns [#14293](https://gitlab.haskell.org/ghc/ghc/-/issues/14293) [#22406](https://gitlab.haskell.org/ghc/ghc/-/issues/22406).

- Refactoring of rebindable syntax; see [wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/Rebindable-syntax).

- Propagate more information about patterns in the renamer to help
  record update disambiguation [#23032](https://gitlab.haskell.org/ghc/ghc/-/issues/23032)
  [#22746](https://gitlab.haskell.org/ghc/ghc/-/issues/22746).

