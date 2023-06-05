<!--
Compile the HTML slides with Pandoc:

pandoc -t revealjs -s -o index.html renamer.md -V revealjs-url=reveal.js -V theme=welltyped_theme -V controlsTutorial=false -V controlsLayout=edges -V slideNumber='c/t' -V transition=slide

Custom theme in reveal.js/dist/themes/welltyped_theme.css
-->

---
author: Sam Derbyshire, Well-Typed
title: GHC's renamer
subtitle: GHC Contributors' Workshop
date: June 7th, 2023
---

<p align="center">
<img src=GHC_passes_renamer.svg height="500px" />
</p>

## Plan

- First part: survey of the renamer and how information about
  identifiers flow through the compiler pipeline.
- Second part: getting our hands dirty, fixing a bug in the renamer
  (interaction of `COMPLETE` sets of pattern synonyms and `do` notation).

<p class="indicator">⭲</p>

## What does the renamer do?

<ol type="I">

  <li class="fragment" data-fragment-index="1">
    Gives all names unique identifiers.
    <ul>
      <li>
        Resolve namespacing (type constructor vs variable, module qualification,
        record fields).
      </li>
      <li>
        Handle shadowing.
      </li>
    </ul>
  </li>

  <li class="fragment" data-fragment-index="2">
    Usage & dependency analysis.
    <ul>
      <li>
        Redundant/unused imports, unused declarations, ...
      </li>
      <li>
        Dependency analysis: determines order of typechecking.
      </li>
      <p class="indicator">⭲</p>
    </ul>
  </li>

</ol>

## Names

There are many ways that GHC can refer to an identifier depending on how
much it knows about it.

:::{.element: class="fragment"}
  - `OccName`
:::
:::{.element: class="fragment"}
  - `Name`
:::
:::{.element: class="fragment"}
  - `RdrName`
:::
:::{.element: class="fragment"}
  - `GlobalRdrElt`
:::
:::{.element: class="fragment"}
  - `Var`, `TyVar`, `Id`
<p class="indicator">⭲</p>
:::



## `OccName`: occurrences with a `NameSpace`

:::{.element: class="fragment"}
```haskell
data OccName = OccName
    { occNameSpace :: NameSpace
    , occNameFS    :: FastString }

data NameSpace
  = VarName
  | FldName { fldParent :: FastString }
  | DataName
  | TvName
  | TcClsName
```

<p class="indicator">⭲</p>
:::

## `Name`: unique identifiers

:::{.element: class="fragment"}
```haskell
data Name = Name
  { n_sort :: NameSort
  , n_occ  :: OccName
  , n_uniq :: Unique
  , n_loc  :: SrcSpan
  }

data NameSort
  = External Module
  | WiredIn Module TyThing BuiltInSyntax
  | Internal
  | System
```

<p class="indicator">⭲</p>
:::


## `RdrName`: umbrella datatype

:::{.element: class="fragment"}
```haskell
data RdrName
  = Unqual OccName
  | Qual ModuleName OccName
  | Orig Module OccName
  | Exact Name
```

<p class="indicator">⭲</p>
:::

## `Var` and `Id`: variables

:::{.element: class="fragment"}
```haskell
data Var -- slightly abridged
  = TyVar { varName    :: Name
          , varType    :: Kind }
  | TcTyVar { varName       :: Name
            , varType       :: Kind
            , tc_tv_details :: TcTyVarDetails }
  | Id { varName    :: Name
       , varType    :: Type
       , varMult    :: Mult
       , idScope    :: IdScope
       , id_details :: IdDetails
       , id_info    :: IdInfo
       }
data IdScope = GlobalId | LocalId ExportFlag
data ExportFlag = NotExported | Exported
data IdDetails = VanillaId | RecSelId {} | PrimOpId {} | CoVarId {} | ...

type TyVar = Var
type Id = Var
```
:::


## Information about `Name`s: `TyThing` and `GREInfo`

:::{.element: class="fragment"}
```haskell
data TyThing
  = AnId     Id
  | AConLike ConLike
  | ATyCon   TyCon
  | ACoAxiom (CoAxiom Branched)
```
:::

:::{.element: class="fragment"}
```haskell
data GREInfo
  = Vanilla
  | IAmTyCon    (TyConFlavour Name)
  | IAmConLike  ConInfo
  | IAmRecField RecFieldInfo
```

<p class="indicator">⭲</p>
:::

## `GlobalRdrElt` and the `GlobalRdrEnv`

The renamer primarily deals with `GlobalRdrElt`, which consists of a `Name`,
information about how it's in scope in the renamer, and additional information
that the renamer might need to know.

:::{.element: class="fragment"}
```haskell
data GlobalRdrElt
  = GRE { gre_name :: Name
        , gre_par  :: Parent
        , gre_lcl  :: Bool
        , gre_imp  :: Bag ImportSpec
        , gre_info :: GREInfo }

type GlobalRdrEnv = OccEnv [GlobalRdrElt]
```

<p class="indicator">⭲</p>
:::


## The `TcRn` monad

Renaming and typechecking happens in a shared monad,
`TcM` (also called `TcRn` or `RnM`).

:::{.element: class="fragment" data-fragment-index="1"}
```haskell
type TcRnIf a b = IOEnv (Env a b)
type TcRn = TcRnIf TcGblEnv TcLclEnv
type TcM = TcRn
type RnM = TcRn
```
:::

:::{.element: class="fragment" data-fragment-index="2"}
This is `ReaderT` over `IO`, with access to:

  - `HscEnv`
      <i class="fragment" data-fragment-index="3"> – per-module options (e.g. flags passed to GHC) and environment</i>  
      <i class="fragment" data-fragment-index="4">e.g. `UnitEnv`, currently loaded modules</i>
  - `TcGblEnv`
      <i class="fragment" data-fragment-index="5"> – generated during typechecking and passed on.</i>  
    <i class="fragment" data-fragment-index="6">e.g. `TypeEnv`, `InstEnv`, `GlobalRdrEnv`</i>
  - `TcLclEnv`
    <i class="fragment" data-fragment-index="7"> – changes as we move inside expressions</i>  
    <i class="fragment" data-fragment-index="8">e.g. `SrcSpan`, `TcLevel`, `LocalRdrEnv`.<p class="indicator">⭲</p></i>
:::

## Example: renaming a record field update

:::{.element: class="fragment"}
Start by looking at `rnExpr (RecordUpd {..})`:
:::

:::{.element: class="fragment"}
```haskell
rnExpr (RecordUpd { rupd_expr = L l expr, rupd_flds = rbinds })
  = setSrcSpanA l $
    case rbinds of
      RegularRecUpdFields { recUpdFields = flds } ->
        do  { (e, fv_e) <- rnExpr expr
            ; (parents, flds, fv_flds) <- rnHsRecUpdFields flds
            ; ... }
      -- ...
```

<p class="indicator">⭲</p>
:::


## Looking up fields

After handling duplicates, `rnHsRecUpdFields` starts by calling `lookupRecUpdFields`
to look up each field individually.

:::{.element: class="fragment"}
```haskell
lookupRecUpdFields :: NE.NonEmpty (LHsRecUpdField GhcPs GhcPs)
                   -> RnM (NE.NonEmpty (HsRecUpdParent GhcRn))
lookupRecUpdFields flds
  = do { gre_env <- getGlobalRdrEnv
       ; fld1_gres NE.:| other_flds_gres <-
           mapM (lookupFieldGREs gre_env . getFieldUpdLbl) flds
       ; -- ...
       }
```

:::

:::{.element: class="fragment"}
We retrieve `GlobalRdrElt`s for each record field.  
These have `GREInfo`s which specify which constructors have that field.  
We can use that to disambiguate.

<p class="indicator">⭲</p>
:::

## Disambiguating using constructors

:::{.element: class="fragment"}
```haskell
data S = MkS1 { x, y :: Float } | MkS2 { x, y, z :: Float }
data T = MkT1 { x :: Word } | MkT2 { y :: Int }

foo r = r { x = 3, y = 4 }
```
:::

:::{.element: class="fragment"}
Lookup: `[(x, [MkS1, MkS2, MkT1]), (y, [MkS1, MkS2, MkT2])]`.
:::

:::{.element: class="fragment"}
Intersect all the possible data constructors: `[MkS1, MkS2]`.
:::

:::{.element: class="fragment"}
Take parents (removing duplicates): `[S]`.
:::
:::{.element: class="fragment"}
There is a single parent: the record update is unambiguous!

<p class="indicator">⭲</p>
:::

## Flow through the compiler & trees that grow


:::{.element: class="fragment"}
```haskell
type family IdP p
type LIdP p = XRec p (IdP p)
type family XRec p a
```
:::

:::{.element: class="fragment"}
```haskell
type instance IdP (GhcPass p) = IdGhcP p
type family IdGhcP pass where
  IdGhcP 'Parsed      = RdrName
  IdGhcP 'Renamed     = Name
  IdGhcP 'Typechecked = Id
```
:::

:::{.element: class="fragment"}
```haskell
type instance XRec (GhcPass p) a = GenLocated (Anno a) a

data GenLocated l e = L l e

type instance Anno RdrName = SrcSpanAnnN -- SrcSpan, but with
type instance Anno Name    = SrcSpanAnnN -- some extra stuff
type instance Anno Id      = SrcSpanAnnN -- for exact-print
```
<p class="indicator">⭲</p>
:::

## Lexing

We start off by lexing. See `GHC.Parser.Lexer.x`:

:::{.element: class="fragment"}
```ocaml
@varid  = $small $idchar*          -- variable identifiers
@conid  = $large $idchar*          -- constructor identifiers
@varsym = ($symbol # \:) $symbol*  -- variable (operator) symbol
@consym = \: $symbol*              -- constructor (operator) symbol

$small = [a-z \_]
$large = [A-Z]
symbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]
$idchar = [$small $large $digit \']
```
<p class="indicator">⭲</p>
:::

## Parsing

Then parsing, in `GHC.Parser.y`.

<ul>

  <li class="fragment">
    Parse occurrences into `RdrName`.
    <ul>
      <li class="fragment">
        Use `mkQual`/`mkUnqual` depending on qualification.
      </li>
      <li class="fragment">
        Resolve `NameSpace` using context (e.g. whether we are parsing a term
        or a type).
      </li>
    </ul>
  </li>

  <li class="fragment">
    Fix up after the fact when necessary.
  </li>

</ul>

:::{.element: class="fragment"}
Example: parsing a type constructor.
```haskell
tyconsym :: { RdrName } -- (actually LocatedN RdrName)
  : CONSYM { mkUnqual tcClsName (getCONSYM $1) }
  | VARSYM { mkUnqual tcClsName (getVARSYM $1) }
  | ':'    { consDataCon_RDR }
  | '-'    { mkUnqual tcClsName (fsLit "-") }
  | '.'    { mkUnqual tcClsName (fsLit ".") }
```

<p class="indicator">⭲</p>
:::

## Quick example of fixing up

:::{.element: class="fragment"}
```haskell
data D a b c d = Q a => b :+: c :*: d
```
:::

:::{.element: class="fragment"}
Parse this as a type first. Then, after properly associating using the
fixities, we find that `:+:` should be namespaced as a data constructor,
and the others remain type constructors.
:::

:::{.element: class="fragment"}
We also need to resolve ambiguities between expression and patterns.
See Note [Ambiguous syntactic categories] in `GHC.Parser.PostProcess`.
<p class="indicator">⭲</p>
:::

## The renamer & typechecker pipeline

:::{.element: class="fragment"}
The main entry point is `GHC.Tc.Module.tcRnModuleTcRnM`.
:::

<ol type="I">

  <li class="fragment">
    Rename the import list in `tcRnImports`.
  </li>

  <li class="fragment">
    Rename and typecheck local declarations and the export list in `tcRnSrcDecls`.
    <ol type="i">
      <li class="fragment">
        Rename local declarations in `rnTopSrcDecls`.
      </li>
      <li class="fragment">
        Typecheck these declarations in `tcTopSrcDecls`.
      </li>
      <li class="fragment">
        Rename the exports in `rnExportList`. This allows us to assemble
        a final `TcGblEnv` which contains everything provided by the module.
      </li>
    </ol>
  </li>

  <li class="fragment">
    Assemble the final typechecked module, to be passed onto the
    next stage of the compiler pipeline. Everything is extracted from
    the `TcGblEnv`.

    <p class="indicator">⭲</p>
  </li>

</ol>


## Renaming imports

Entry point: `GHC.Rename.Names.rnImportDecl`.

<ol type="I">

  <li class="fragment">
    Load the module we're importing; this will load its interface file
    from disk, or directly load the information if it's available in memory.
  </li>

  <li class="fragment">
    Figure out what we are importing, in `filterImports`.
    <ul>
      <li class="fragment">
        For a blanket import, import everything.
      </li>
      <li class="fragment">
        For an explicit import list, accumulate everything that is mentioned.
        <p class="fragment">
        `import M( A(x, ..), B(..) )`
        </p>
      </li>
      <li class="fragment">
        `import M hiding ( .. )`
      <p class="fragment">
        Works the same, except now we filter out instead.
      </p>
      </li>
    </ul>
  </li>

  <li class="fragment">
    Add all the imported identifiers to the `GlobalRdrEnv`. We will look up
    in this environment when renaming the body of the module.

    <p class="indicator">⭲</p>
  </li>

</ol>

## Renaming local definitions

:::{.element: class="fragment" data-fragment-index="1"}
The main entry point to renaming local declarations is `GHC.Rename.Module.rnSrcDecls`.
Control flow:
:::

<ol type="I">
  <li class="fragment" data-fragment-index="3">
    Generate new `Name`s for all binders.
    <span class="fragment fade-out" data-fragment-index="11">
    <ol type="a">
      <li class="fragment" data-fragment-index="4">
      "Non-value" binders `getLocalNonValBinders`.
        <ol type="i">
          <li class="fragment" data-fragment-index="5">
            Type synonyms, type families, data declarations, data families,
            class declarations.
          </li>
          <li class="fragment" data-fragment-index="6">
            Data family instances and class instances, including associated types
            and methods.
          </li>
          <li class="fragment" data-fragment-index="7">
            Foreign imports.
          </li>
         </ol>
      </li>
      <li class="fragment" data-fragment-index="8">
        Pattern synonyms `extendPatSynEnv`.
      </li>
      <li class="fragment" data-fragment-index="9">
        Generate new `Name`s and rename the LHS of top-level value bindings
        (`rnTopBindsLHS`).
      <p class="indicator">⤞</p>
      </li>
    </ol>
    </span>
  </li>

  <li class="fragment" data-fragment-index="11">
    Rename declaration bodies.
    <ol type="a">
      <li class="fragment" data-fragment-index="12">
        Type declarations `rnTyClDecls`.
        <ol type="i">
          <li class="fragment" data-fragment-index="13">
            Type synonyms, type families, data declarations, data families,
            class declarations.
          </li>
          <li class="fragment" data-fragment-index="14">
            Standalone kind signatures.
          </li>
          <li class="fragment" data-fragment-index="15">
            Type family instances, data family instances, class instances.
          </li>
          <li class="fragment" data-fragment-index="16">
            Role annotations.
          </li>
        </ol>
      </li>
      <li class="fragment" data-fragment-index="17">
        Top-level value bindings `rnValBindsRHS`.
      </li>
      <li class="fragment" data-fragment-index="18">
        Everything else: `RULES`, foreign import/exports, default declarations...
        <p class="indicator">⭲</p>
      </li>
    </ol>
  </li>

</ol>


## Dependency analysis of type declarations

:::{.element: class="fragment"}
`rnTyClDecls`
  - renames all types/classes defined in the module
  - uses this information to compute dependency groups (strongly-connected components).
components.
:::


:::{.element: class="fragment"}
```haskell
rnTyClDecls :: [TyClGroup GhcPs] -> RnM ([TyClGroup GhcRn], FreeVars)
rnTyClDecls tycl_ds =
  do { tycls_w_fvs  <- mapM rnTyClDecl (tyClGroupTyClDecls tycl_ds)
     ; kisigs_w_fvs <- rnStandaloneKindSigs (tyClGroupKindSigs tycl_ds)
     ; instds_w_fvs <- mapM rnSrcInstDecl (tyClGroupInstDecls tycl_ds)
     ; let tycl_sccs = depAnalTyClDecls kisig_fv_env tycls_w_fvs
     ; .. }
```
:::


:::{.element: class="fragment"}
See Note [Dependency analysis of type, class, and instance decls] in `GHC.Rename.Module`.
:::

<br/>

:::{.element: class="fragment"}
This approach is known to have shortcomings, as it doesn't properly account for
type instances when kind-checking. <p class="indicator">⭲</p>  
See [wiki page: Type-&-Class-Dependency-Analysis](https://gitlab.haskell.org/ghc/ghc/-/wikis/Type-&-Class-Dependency-Analysis).

:::

## Dependency analysis of value declarations

:::{.element: class="fragment"}
```haskell
data HsBindLR idL idR
  = FunBind {..} -- used for function/variable bindings
  | PatBind {..} -- (pattern is never a simple variable)
  | VarBind {..} -- (introduced by the typechecker)
  | PatSynBind {..}
  | XHsBindsLR !(XXHsBindsLR idL idR) -- (introduced by the typechecker)
```
:::

:::{.element: class="fragment"}
`rnValBindsRHS`
  - renames all value bindings
  - performs dependency analysis
:::


:::{.element: class="fragment"}
```haskell
rnValBindsRHS ctxt (ValBinds _ mbinds sigs)
  = do { (sigs', sig_fvs) <- renameSigs ctxt sigs
       ; binds_w_dus <- mapBagM (rnLBind (mkScopedTvFn sigs')) mbinds
       ; let !(anal_binds, anal_dus) = depAnalBinds binds_w_dus
       ; ... }
```

<p class="indicator">⭲</p>
:::

## Topics not covered

:::{.element: class="fragment"}
- Template Haskell
:::
:::{.element: class="fragment"}
- Backpack
:::

##

Slides available online: [sheaf.github.io/ghc-renamer](https://sheaf.github.io/ghc-renamer).
