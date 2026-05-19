# Project rules for code-police

## document-symbols

Top-level symbols carry Haddock comments. The bar differs by visibility:

- **Every exported symbol** is documented — no exceptions. Multi-concern export lists are organized into named groups so the public API reads like a table of contents.
- **Internal symbols** are documented when their purpose isn't obvious from name and type alone. A type that mirrors an external JSON shape, a TH-spliced binary path, a record whose fields encode a non-trivial protocol — those want a Haddock. A trivial alias or one-line helper doesn't.

Only export a symbol if something outside the module imports it. Dead exports are dead code with an extra step.

_How to apply_:

- Above each exported declaration (function, type, instance, etc.), write a `-- |` Haddock describing what it is and why a caller would use it. Exception: a single-function module whose module-level Haddock already describes that function (typical `Main`) doesn't need the per-function repeat.
- For internal symbols, ask: would a reader who didn't write this know what this is for from name and type alone? If not, write the Haddock.
- When a module exports more than a couple of names across distinct concerns, organize the export list with Haddock group headings (`-- * Group name`).
- Don't export a name unless something outside the module imports it. Grep the codebase; if nobody imports `foo`, drop it from the export list (and probably the binding — see `no-dead-code`).

_Anti-patterns_:

- An exported declaration with no Haddock above its type signature.
- An internal data type whose purpose requires reading consumer code to understand.
- A multi-concern export list with no group headings.
- Exports that nothing imports.

## one-module-one-concern

Each `.hs` module owns a single concern. Don't pile unrelated types, functions, and IO into one module just because they're new — group code by what changes together, not by what's convenient to type into one file.

_How to apply_:

- Ask "what's the one sentence that describes this module?" If you can't say it without "and", split.
- Generic algorithms (reachability, encoding, formatting) live in their own module so they can be reused or replaced without churn in unrelated code.
- IO and shell-outs to a specific external tool live in that tool's module — not the entry point.

_Anti-patterns_:

- One file holding: schema types + FromJSON instances + Template-Haskell binary lookup + BFS algorithm + JSON re-emitter + `main`. Each is a different rate of change.
- A `Utils.hs` / `Common.hs` that grows by accretion. If you can't name its single concern, it's a dumping ground.

## prefer-aeson-auto-derive

Default to Generic-based auto-derivation (`deriving stock Generic` + `deriving anyclass FromJSON`/`ToJSON`). Hand-write `parseJSON`/`toJSON` only when auto-derivation genuinely can't express the mapping. **"Can't express" is a question about whether *any* shape of the type would let Generic emit the wire — not about whether the type as currently written can.** If you're hand-rolling because a field needs to be wrapped, projected, or indexed at serialization time, the field's *type* is what's wrong, not aeson's expressivity.

_How to apply_:

- **Name Haskell record fields to match the JSON keys.** aeson Generic ignores unknown fields, so missing keys aren't a parse error. Each omission is a deliberate design choice though — verify the field has no domain meaning your consumer needs before deciding it's ignorable. "Ignore" should mean "I read the source schema and this is runtime metadata", not "I haven't looked at what this field is for".
- Mirror the wire structure as records; write a `data` → `data` projection function for any consumer that wants flattened data. The parser stays generic.
- Only if the JSON keys can't match the Haskell field names (reserved words, casing constraints), use `genericParseJSON` with an `Options { fieldLabelModifier = ... }` modifier. Still derives, just with a transform.
- **If a hand-rolled `toJSON` is transforming a field's shape** — wrapping a collection into objects, fabricating a constant key per element, projecting a sub-field, indexing by name — **ask whether the field's type should change so Generic can emit it directly.** Promoting the value into a named record costs one type declaration and removes the manual instance forever; the wire is the same, the blast radius for the next schema change shrinks.
- For closed sums whose constructor names don't match the wire (typically `CamelCase` constructors ↔ `snake_case` strings), keep the auto-derive and parameterize the `Options` once: `defaultOptions { constructorTagModifier = camelTo2 '_', tagSingleConstructors = True }`. The `tagSingleConstructors = True` is load-bearing for single-constructor nullary sums — without it aeson emits an empty array instead of the tag string. A shared `Options` value beats N hand-rolled per-constructor `toJSON` clauses.
- Singleton-sum smell isn't fixed by inlining the wire literal as a `Text` constant. It's fixed by making the sum a *real* sum — name every wire variant the spec admits in the closed type, even the ones you don't emit yet. The type stops being a singleton; the call sites stay type-safe.

_Anti-patterns_:

- Hand-writing `parseJSON = withObject "X" $ \o -> X <$> o .: "name" <*> o .: "age"` when the field names match the JSON. That's the literal contract Generic already gives you.
- Renaming Haskell fields away from the JSON keys "for aesthetics" and then needing `fieldLabelModifier`. Match the wire; rename in the projection layer if anyone needs it.
- Hand-rolling `toJSON` because the field is a `Set`/`[Text]`/`Bool` that has to become a structured object on the wire. The field type is the problem — promote it to a record so Generic does the work.
- Deleting a sum type because today it has one constructor and inlining the wire literal at the call site. The right move is to widen the sum to every variant the schema admits and keep one constructor in active use.

## module-needs-description

Every `.hs` module must carry a top-level Haddock comment naming what the module is for — a single sentence is fine. This is documentation of the module's concern, not commentary on the code (the `comments-only-for-non-obvious` rule still applies inside the body).

_How to apply_:

- Place `-- | <one-line description>` immediately above the `module` declaration.
- For multi-line descriptions, use a `{-| ... -}` block.
- The sentence should be the same answer you'd give to `one-module-one-concern` — "this module is for X". If you can't write that sentence, the module is doing too much.

_Anti-patterns_:

- Restating the module name: `-- | The Main module.`
- Listing exports: `-- | Exports foo, bar, baz.`
- Narrating the diff: `-- | Added in PR #2 for the just-graph feature.`

## organize-exports

Every export list is structured, not flat. Organize it the way a reader would scan it: public API at the top in concern-grouped sections; below a visible divider, the symbols exposed only for tests or for closely-coupled sibling modules. The list is the module's table of contents — the order and the grouping carry meaning.

The minimal shape:

```haskell
module CI.Foo (
    -- * Values
    Foo,
    fooFromText,

    -- * Operations
    doFoo,

    -- * === Internal (exposed for tests) ===
    fooInternalAccessor,
) where
```

This complements `document-symbols` (which mandates per-symbol Haddocks and group headings on multi-concern lists) by adding a fixed *order* and a *visible divider* between public and internal.

_How to apply_:

- Put **public exports first**, grouped under `-- * <Concern>` Haddock headings. "Public" means imported by any module other than `*Spec.hs` test files.
- Below them, place a single divider heading whose label flags the section as non-public — `-- * === Internal ===`, `-- * === Internal (test surface) ===`, or similar. The `===` (or any equivalent visual marker) is what makes the boundary scannable in a diff or a hover preview.
- Every internal export carries a one-line Haddock explaining *why* it's exposed (which test or sibling needs it). Internal isn't a slush bucket — each entry is justified.
- A module with no internal exports omits the divider entirely.
- A module whose export list is short enough to fit on a few lines without distinct concerns can skip the section headings — but if a divider is needed (any internal exports at all), the public side still gets at least one heading so the divider has something to be "below."

_Anti-patterns_:

- A flat export list mixing public types, operations, and test-only helpers in declaration order.
- An "internal" symbol exposed at the top of the list because that's where its definition happens to live in the source.
- An empty `-- * === Internal ===` divider left behind after the last test-only helper was inlined. If the section's empty, delete the heading.
- A divider with no visual marker — just `-- * Internal` reads like another concern group, not a boundary.

## no-partial-functions

Don't reach for functions that can crash on a value of the correct type — `head`, `tail`, `init`, `last`, `fromJust`, `(!!)`, `read`, `Map.!`, `error`, `undefined` — when a total alternative exists. Non-exhaustive pattern matches fall under the same rule: every `case` covers every constructor (use `_` deliberately if you mean "I don't care").

_How to apply_:

- Need the first element? Pattern-match (`x : _`) or take `NonEmpty` and use `Data.List.NonEmpty.head`. Not `Data.List.head`.
- Looking up a `Map` key? `Map.lookup` (returns `Maybe`) or `Map.findWithDefault`. Not `Map.!`.
- Parsing text? `Text.Read.readMaybe`. Not `read`.
- `error`/`undefined` are reserved for invariants the type system can't express. Not for invalid input — that's an `Either`.

_Anti-patterns_:

- `head xs` where `xs` could be empty. Bake the non-emptiness into the type (`NonEmpty`) or pattern-match.
- `case x of Just y -> y` with no `Nothing` branch — `fromJust` in disguise.
- `error "shouldn't happen"` instead of refactoring so the impossibility is typed away (parameterize over the constraining type, or split the function).

## propagate-errors-via-either

Expected, recoverable failures flow through `Either e a` (or `m a` constrained by `MonadError e m`), not into `die`/`error`/`undefined`/`throwIO` deep in the call graph. The boundary — usually `main` or a request handler — is the only place that converts the `Either` to an exit code, log line, or HTTP response.

_How to apply_:

- A function that can fail for an *expected* reason (parse error, missing key, validation, schema mismatch) returns `Either e a` (pure) or `m a` for `MonadError e m` (stack).
- IO actions that can meaningfully fail return `IO (Either e a)` rather than throwing — caller decides whether to die or recover.
- Top-level entry points consume the `Either` once and translate to the appropriate side effect (`die`, `respond 4xx`, etc.).

_Anti-patterns_:

- `loadConfig :: IO Config` that `die`s on a parse failure. Caller can't see this can fail, can't recover, can't write a retry. Should be `IO (Either String Config)`.
- Catching an exception and rethrowing as `error "..."` — that's renaming a partial function, not fixing it.
- `fromRight (error "won't happen") result` — same partial function wearing a hat.

## structured-errors

Error values are structured types, not strings. A function that can fail in distinct ways exposes those failure modes as constructors so callers can pattern-match, log structured fields, or recover programmatically. `String`/`Text` errors collapse all failure paths into one opaque blob — readable in CLI output, useless to any caller that wants to do more than dump and die.

_How to apply_:

- Define a sum type per error domain: `data FetchError = FetchParseError String | NetworkError IOException | ...`. Functions return `Either MyError a` (or `m a` for `MonadError MyError m`).
- The **display layer** — where the error reaches a human — owns the formatting: a `displayError :: MyError -> Text` function in the same module as the error type. The boundary (`main`, a handler) calls it once.
- If the value an upstream library hands you is genuinely just a `String` (e.g. aeson's `eitherDecodeStrict` returns `Either String a`), wrap it in a single-constructor type: `data FetchError = FetchParseError String`. The string survives at runtime; the type system now distinguishes "this is a fetch error" from other error kinds.

_Anti-patterns_:

- `Either String a` or `IO (Either String a)` as a function's return type. Callers can't discriminate failure modes.
- Building a user-facing message string inline at the failure site (`Left ("recipe " <> show k <> " not found")`). The structured error should carry `k`; the display function formats it.
- Catching a structured error and re-throwing as a `String` — same bug, one layer deeper.

## errors-match-callee-failures

A function's error type is its *exact* failure set. Every constructor of the sum it returns must be reachable from that function's body — otherwise the type is overstated and every caller pays the cost: pattern-matches against dead branches, lost exhaustiveness warnings, a reader who can't tell which paths are actually live.

The classic violation is the kitchen-sink `<Module>Error` covering every failure any operation in the module might hit, returned by all of them. A `resolveSomething :: IO (Either ModuleError T)` that advertises a `DirtyState [_]` failure only some *other* function in the module can produce makes every caller handle a case that will never fire — or wildcard it and lose the safety net when branches are later added.

The mechanical check: for each `data XError = C1 | C2 | ...` and each function whose signature is `... -> Either XError _` (or `... -> m _` with `MonadError XError m`), every constructor must be constructed in at least one such function's body. Any constructor unreachable from every function returning the type is a dead branch by construction; any function that can only produce a strict subset is lying.

_How to apply_:

- For each function returning an `Either e _`, enumerate the constructors of `e` its body produces. If the set is a strict subset of `e`'s constructors, the type is overstated. Either split `e`, or return the smaller subset.
- Prefer **per-function focused error types**: one error sum whose constructors are exactly that function's failure modes. Two functions sharing one failure mode share a constructor by *composition* (wrapping), not by *inheritance* (a shared god-type).
- If a function has a single failure mode that already has a focused type (e.g. `SubprocessError`), return that directly. Don't wrap it in a single-constructor module-level sum for naming symmetry.
- Cross-function composition uses constructor wrapping: `data OuterError = OuterParse String | OuterInner InnerError`. The wrapper's constructors reflect this layer's actual failures, with `InnerError` carried in one of them.

_Anti-patterns_:

- `data ModuleError = OpA_Failed _ | OpB_Failed _ | OpC_Failed _` returned by all three operations, when `opA` can only produce `OpA_Failed`. Three focused types beat one shared type with two-thirds dead branches at every call site.
- `f :: IO (Either ModuleError T)` whose body never constructs half the constructors of `ModuleError`. The signature lies.
- Wrapping a single-constructor failure mode in a per-module sum just to keep naming uniform across modules. If `Either SubprocessError T` says everything truthful, that's the right type.
- Reading "sum type per error domain" (from `structured-errors`) as "one type per module." Domain is the *function's failure set*, not the *module*.

## use-record-dot

Enable `OverloadedRecordDot` for `r.field` syntax in modules that define or read records. Use plain `r.field` for reads; don't write one-line wrapper functions whose entire body is a single field access, and don't reach for sectioned-functor forms when a plain expression works.

Related extensions to enable per module as needed:

- `NoFieldSelectors` — suppress the auto-generated `field :: Record -> Field` selector functions so dot access is the only path. Keeps the namespace clean.
- `DuplicateRecordFields` — allow multiple records in the same module to share field names. Dot syntax disambiguates by the value's type.
- `RecordWildCards` — `Foo {..}` brings all fields into scope at construction or pattern-match. Useful for records with many fields.

_How to apply_:

- Use `r.field` for direct reads. Pattern-match (or `RecordWildCards`) when destructuring; record-update syntax (`r { f = v }`) for updates.
- For mapping field access over a structure, use a list comprehension (`[d.recipe | d <- r.deps]`) or an explicit lambda. Plain syntax beats clever syntax.
- Inline trivial accesses at the call site. A function whose body is a single field selection is the auto-generated selector under another name.

_Anti-patterns_:

- A module exporting `getFoo :: Bar -> Foo` whose body is `\b -> b.foo`. That's the selector renamed.
- `(.field) <$> r.items` — sectioned-functor composition. Prefer `[i.field | i <- r.items]` (plain dot accesses, no sections) or a `\i -> i.field` lambda.
- Reaching for `foo bar` (function-call style) when `bar.foo` works in scope.

## prefer-newtype-over-string

Domain identifiers and values typed as `Text`/`String` should be wrapped in newtypes — **and the wrapping must be opaque**. A newtype with its constructor exported (`Foo (..)`) is a type alias with extra syntax: any caller can mint a `Foo` from arbitrary `Text`, or pattern-match to strip the type and get raw `Text` back. The compiler stops protecting you the moment either escape hatch is open.

The newtype's public surface is three things, all in the defining module: a controlled set of **smart constructors** (`IsString` for literals, named functions for parsed/composed values), a `Display` instance as the **canonical destructor**, and **typed operations** that consume the value without unwrapping it.

_How to apply_:

- Wrap each domain concept in a positional `newtype RecipeName = RecipeName Text` (no field accessor). Derive `Eq`, `Ord`, `FromJSON`/`ToJSON`, and — for newtypes used as `Map` keys — `FromJSONKey`/`ToJSONKey` via `deriving newtype`. Runtime cost is zero.
- **Export just the type name (`Foo`), never `Foo (..)`.** The constructor stays inside the defining module.
- **For literal-driven construction**: derive `IsString` so callers write `"ci" :: RecipeName` under `OverloadedStrings`.
- **For parse- or policy-driven construction**: define a named smart constructor in the same module — e.g. `parseSha :: Text -> Maybe Sha`, `contextFrom :: Text -> Context`, `viewRepo :: IO (Either GhError Repo)`. That function is the only entry besides `IsString`; the constructor remains unexported.
- **Destructure through `Display`, not the constructor.** Derive `deriving newtype Display` so consumers write `display foo` to get back to `Text`. Pattern-matching the constructor outside the defining module is forbidden — that's a `(Foo x) <- whatever` accessor in disguise.
- **Records of newtypes, not records of `Text`.** A `data Foo = Foo { bar :: Text, baz :: Text }` whose fields are themselves domain identifiers is the same smell as a bare `Text` parameter. Each field is its own newtype; the record composes them.
- Refactor signatures from `Text -> Map Text Foo -> Map Text [Text]` to `Name -> Map Name Foo -> Map Name [Name]` so the compiler catches swapped parameters.

_Anti-patterns_:

- Exporting `Foo (..)`. The constructor leaks the type-laundering API; the newtype is now decorative.
- Exporting `unRecipeName :: RecipeName -> Text` (or similar record accessors). Same hole with a different shape.
- Pattern-matching on the constructor outside the defining module to extract the inner value. Go through `display`, never the constructor.
- A `data Foo = Foo { bar :: Text, baz :: Text }` whose fields are themselves domain identifiers. Bare `Text` in a public record is the same smell as a bare `Text` parameter.
- `f :: Text -> Map Text Foo -> Either Text Bar` where the three `Text`s mean different things.
- A `String` filepath, URL, ID, or token threaded as a plain `String`. If it has a domain meaning, it has a newtype.

## use-conventional-base-types

Pick the conventional Haskell base type for each foundational domain before reaching for a newtype. Substituting one for another adds noise — every reader has to guess whether your `Text` is caller content, a URL, a filesystem path, or raw subprocess output — and forces conversions at every IO boundary. Each `T.pack` / `T.unpack` shimming a value between a string-like field and a stdlib function that wants a different shape is the rule violation made visible.

The default mapping:

| Domain                                  | Base type    |
|-----------------------------------------|--------------|
| Filesystem paths                        | `FilePath`   |
| Free-form text (logs, prose, wire str)  | `Text`       |
| Raw bytes                               | `ByteString` |
| Subprocess argv                         | `[String]`   |
| Domain identifiers (IDs, names, refs)   | newtype      |

When a value carries domain meaning, wrap the base type in a newtype — `prefer-newtype-over-string` covers the wrapping rules. This rule is about *which base type* sits inside (and which base type to use when no newtype is warranted yet).

_How to apply_:

- Filesystem paths use `FilePath`. `System.Directory`, `System.Process`, and `System.FilePath` all consume and produce `FilePath`; storing a path as `Text` forces a conversion at every IO call site.
- Records whose fields encode an external wire format mirror that format's vocabulary at the *value* level, but pick the right Haskell base type at the *field* level: a wire @string@ field that semantically holds a path is `FilePath`, not `Text`. The JSON output is identical either way; the Haskell type is the documentation.
- Subprocess argv stays `[String]` because `System.Process` consumes `[String]`. Don't push it to `[Text]` and unpack at the call site.
- The mechanical check is grep: search for `T.pack`/`T.unpack` adjacent to identifiers shaped like paths (`*Dir`, `*File`, `*Path`, `dir`, `path`, `cwd`). Each hit is either a Text-where-FilePath bug or an intentional crossing worth justifying.

_Anti-patterns_:

- A field typed `Maybe Text` holding a filesystem path (`working_dir`, `log_file`, `cwd`). Use `Maybe FilePath`.
- `T.pack <$> somePath` or `T.unpack pathField` bridging the wrong type at an IO boundary. The conversion is the bug.
- `[Text]` whose values are filesystem paths. Use `[FilePath]`.
- `[Text]` named `paths` whose values are **not** pure paths (e.g. `git status --porcelain` lines that include status flags). Either rename the variable to reflect the real content (`lines`, `entries`), or parse the path out and use `[FilePath]`. Type and name must agree on what's inside.
- Mixing the conventions across a single domain — some `working_dir` fields `FilePath`, others `Text`. Pick the right type once and use it everywhere.

## main-is-thin

`src/Main.hs` is the harness: argv → parse → dispatch → exit. Anything more — orchestration, mode-specific IO, runtime-artifact layout, domain records, binary-path lookups — moves into a sibling module so adding a feature doesn't fatten Main.

_How to apply_:

- Main hosts at most: the command sum, the parser builder, `main`, and (optionally) a tiny boundary helper if no other module owns it.
- Per-mode bodies, records that describe runtime artifacts, and assembly functions that walk multiple subsystems all live in a sibling module — not in Main.
- Heuristic: if Main exceeds ~70 lines (imports + command sum + parser + main + dispatch + one tiny helper), check whether a new function or record snuck in that belongs elsewhere.

_Anti-patterns_:

- Per-mode orchestration functions defined in Main.
- Domain records, path conventions, command-builders defined in Main.
- A runtime-artifact record threaded through Main's helpers — should live in the module that owns the convention.

## encapsulation-passes-grep

A module that claims to own an interface — a CLI binary, a foreign API, a wire protocol, a low-level handle, an internal subsystem — must be the only file that touches its raw primitives. The module name, top-level haddock, and export list are *claims* about a boundary; the import graph is the *test* of that boundary. When the two disagree, the boundary doesn't exist — only the label does.

The mechanical check is grep. For each raw primitive a wrapper module exposes (binary path, untyped handle, low-level identifier — anything that sits *below* the wrapper's typed operations), grep the codebase for it. Exactly one file should mention the primitive: the wrapper itself. Two or more is a falsified encapsulation, full stop. No labels, no narratives, no judgement — the grep returns 1 or it doesn't.

_How to apply_:

- For each module whose stated concern is "wrap interface X", enumerate the raw primitives it exports alongside its typed operations.
- Grep the codebase for each primitive. One consumer (the wrapper) passes. Two or more consumers means the encapsulation is fiction.
- When grep returns more than one file, pick one: (a) lift the second caller's usage into a new typed operation on the wrapper, then route the caller through it; (b) demote the wrapper's claim — rename it or rewrite its haddock so it no longer pretends to own the interface.
- Treat haddock cross-references between sibling modules as *confessions*, not documentation. Phrases like "the other half lives in X", "shared from here", "see also X for the actual call" are reports that one concern is split across two modules. Run the grep before accepting the split as intentional.
- Run this check before approving any change that adds a consumer of a wrapper module's primitive, and on every PR that touches a wrapper module.

_Anti-patterns_:

- A wrapper module exports both typed operations *and* the raw primitive, and a consumer module imports the raw primitive and rebuilds the same shape of call the wrapper was supposed to encapsulate.
- A wrapper module retains only an ancillary operation (a discovery query, a ping, a health check) while the principal operation lives in a consumer module that reaches past the wrapper. The label outlives the encapsulation.
- Justifying a leaked primitive with "only one other call site needs it" or "the second consumer is just composing." Composition consumes the wrapper's typed API; reaching for the primitive *is* bypass, not composition. Call-site count is irrelevant — the concern is split or it isn't.
- Evaluating a module's boundary from its haddock, name, and exports without checking who imports it and what they do with it. The surface is written by the boundary's author and will always agree with the boundary; the import graph is written by consumers and tells you what they actually needed.

## code-style

Small style conventions. Each bullet is mechanically checkable.

- **Prefer `$` to nested `(..(..)..)`** — `f $ g $ h x` reads more linearly than `f (g (h x))`. Use parens only when precedence genuinely demands them.
