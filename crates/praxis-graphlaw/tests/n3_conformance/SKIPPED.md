# Skipped EYE `reasoning/` directories

One line per skipped directory: what it is, and the concrete, verified reason
it was not vendored. Each reason below was confirmed by actually attempting
to parse/materialize the real upstream `.n3` source with this engine (via
`Parser::parse_n3_document` / `TripleStore::materialize`), not assumed.

**Update (post-verification-pass follow-up):** four of the sub-gaps
catalogued below have since been closed for real: `n3.pest`'s `Subject`
production now accepts a bare `Literal` (closing the
"literal-as-subject"/`true log:callWithCut true.`-shape parse errors
documented for `ackermann`/`ill-formed-literals`/`proof-by-induction`/
`goldbach`/`kaprekar`, though those four directories each depend on at least
one *other*, still-open gap independently -- see their entries below, not
yet re-verified/re-vendored); `math:notEqualTo` and `list:notMember`/
`list:firstRest` are now registered builtins (see `lib/tests/
n3_new_builtins.rs`), closing `path-discovery`'s *only* documented blocker --
that directory is now a real candidate for vendoring (modulo its 4.8 MB
payload needing truncation), not yet done as part of this update. Also new:
any predicate from a recognized N3/EYE builtin namespace (`log:`/`math:`/
`string:`/`list:`/`time:`/`crypto:`/`func:`/`e:`) that still isn't registered
-- `log:callWithCut`, `log:repeat`, `e:findall`, etc. -- now makes the engine
panic with a clear message instead of silently treating the rule as never
firing (`builtins::reject_if_unsupported_builtin`, `lib/tests/
n3_unsupported_builtin_guard.rs`). This makes every remaining gap below
loud/detectable at runtime rather than a silent wrong answer, even before
each directory is individually re-visited and re-vendored.

- `bnode-scope`: genuinely unsupported syntax. `test1.n3` embeds a full
  nested rule (`{...} => {...}.`) *inside the consequent* of another rule
  (`... => {{...} log:collectAllIn _:x} => {:a :n :m}}`). This engine's
  grammar (`n3.pest`) only allows `Head = "{" TP+ "}" | DenyHead` inside a
  rule's consequent -- a `TP` is a plain triple pattern, not another full
  `rule` (`=>`/`<=`) production, so a rule-within-a-rule's head cannot
  parse. Confirmed: `Parser::parse_n3_document` returns a real parse error
  at the nested `=>` (`N3 parse error --> 10:73`), and falls back to the
  legacy line-splitting parser, which silently mis-parses the file into a
  meaningless partial fact set rather than erroring -- so this directory
  cannot be vendored without either faking a result or fixing a real,
  separate grammar gap (nested-rule-as-consequent) that is out of scope for
  this vendoring batch.
- `reif`: genuinely unsupported syntax on both sides of the reify/dereify
  round-trip this directory demonstrates. `reify.n3`/`dereify.n3` both use
  EYE's `true <= { ... }` idiom (a bare `true`/`false` keyword standing in
  for a rule's Head/consequent) plus a custom `e:becomes` bidirectional-rule
  predicate between two quoted formulas -- this engine's grammar only
  accepts `Head = "{" TP+ "}" | DenyHead("false")`, so a bare `true` as
  Head is not a valid production at all. Confirmed: `Parser::parse_n3_document`
  errors at line 4 of both files (`expected EOI, Prefix, Base, SparqlPrefix,
  SparqlBase, ForAll, ForSome, Keywords, TP, or rule`). Since neither
  transformation rule parses, there is no working rule left to
  materialize()-derive a real result from (`data.n3`/`data-reif.n3` on
  their own are just plain facts with no reasoning to check), so the
  directory has nothing left that this vendoring pass can honestly exercise.
- `ill-formed-literals`: genuinely unsupported syntax. Several of its
  detector rules use a bare numeric literal as the *subject* of a
  `log:notIncludes` scope guard (`1 log:notIncludes { ?lit math:equalTo
  ?lit. }.`, for the xsd:date/dateTime/duration checks). This engine's
  grammar's `Subject` production (`n3.pest`) is `PathExpr | IriRef |
  Prefixed | Var | BlankNode | List | Formula | BNodeProps` -- it does not
  include a bare numeric `Literal`, so a literal-as-subject triple pattern
  cannot parse. Confirmed: `Parser::parse_n3_document` errors exactly at
  that line (`134:3`, `expected ForAll, ForSome, or TP`). The rest of the
  file (boolean/integer/decimal/float lexical-form checks via
  `string:notMatches`) would parse and materialize fine in isolation, but
  vendoring only a hand-edited subset would mean not reproducing the real
  upstream file verbatim, which this batch's rules require -- skipped as a
  whole rather than silently altered.
- `derived-rule`: same genuine unsupported-syntax gap as `bnode-scope`
  above (a full nested rule -- `{ ?y a :Dog. } => { :test :is true. }.` --
  embedded inside the consequent of the outer `{ ?x a :Cat. } => { ... }`
  rule). Confirmed: `Parser::parse_n3_document` errors at the nested `=>`
  (`N3 parse error --> 12:8`, `expected Object`), and, as with
  `bnode-scope`, `TripleStore::from` then silently falls back to the
  legacy line-splitting parser which panics on this multi-line content
  rather than erroring cleanly. This is the same "nested rule as
  consequent" grammar gap as `bnode-scope`, not a second independent one.
- `meta-interpretation`: parses successfully (no grammar gap) but does not
  produce a correct, or even directionally-useful, result -- a genuine
  reasoning-engine limitation, not a corpus/domain-demo exclusion.
  `mi.n3`'s self-interpreter walks a `log:` rule-body-as-list representation
  recursively via `list:firstRest` to prove `() :natnum (:s (:s (0)))`-style
  goals; combined with `mi-query.n3`'s `{() :mi ({() :mi ({() :mi ({()
  :natnum ?X})})})} => {() :natnum ?X}`, EYE's real answer
  (`mi-answer.n3`) derives 5 `:natnum` facts for Peano `0` through
  `:s(:s(:s(:s(0))))`. Actually running `materialize()` on the combined
  input in this engine derives only the intermediate `:mi`/`:headBody`
  scaffold triples for the meta-interpreter's own bootstrapping step (a
  `_:__n3list_N :mi _:__n3list_N` self-loop plus three `:headBody` facts)
  and **zero** `:natnum` triples -- i.e. the `?X`-binding never propagates
  out through the nested meta-circular rule application, so the case's
  actual point (proving four derived Peano numbers via self-interpretation)
  silently fails to derive anything meaningful. This is consistent with the
  plan's documented boundary that bare-variable (non-`@forAll`/`@forSome`)
  scoping is flatly name-matched rather than given real per-formula scope
  (`log_implies.rs`) -- deeply nested formula-quoted rule bodies with
  repeated bare `?X`/`?A`/`?G` reuse across nesting levels are exactly the
  shape that boundary does not handle correctly. Vendoring this case would
  require hand-writing a result that the engine does not actually derive,
  which the task's rules explicitly forbid -- skipped and documented as a
  real gap rather than silently worked around.
- `n3-star`: genuinely unsupported syntax. `example.n3` uses RDF-star
  quoted-triple-as-term syntax `<<( :s :p :o )>>` (both as a fact subject
  and inside a rule body/head pattern, e.g. `<<( ?s ?p ?o )>> ?p2 ?o2`).
  There is no `<<` / `>>` production anywhere in `n3.pest` (`Subject`,
  `Object`, `Literal`, `Term` all lack it), so this is not parseable at
  all, confirmed by inspection of the grammar (not just a parse-error
  guess -- the token literally does not appear in the file).
- `rdf-star`: genuinely unsupported on two independent axes, verified
  against the real `run-manifest.n3`. First, its rules are pure EARL
  test-harness meta-rules that shell out via `e:exec` (`"eye --quiet
  --nope ... 2>&1 | grep ERROR ... > out.txt"`) and read the result back
  via `e:fileString` -- real external-process execution, out of scope for
  a reasoning-conformance suite regardless of grammar support. Second, the
  directory's actual RDF-star payloads (`run-outcome.n3`/
  `run-outcome-pass.n3`, referenced but not fetched given the first
  disqualifier) are exactly the same `<<( )>>` syntax as `n3-star`, so it
  would fail the same grammar gap even ignoring the `e:exec` issue.
- `rdf-star-reasoning`: genuinely unsupported syntax, same root cause as
  `n3-star`. `socrates.n3`'s data and its single subClassOf-propagation
  rule are built entirely on `<<( :Socrates rdf:type :Human )>>`-style
  quoted-triple terms (`:we :have <<( :Socrates rdf:type :Human )>>.` and
  `{?A rdfs:subClassOf ?B. :we :have <<( ?S rdf:type ?A )>>} => {:we :have
  <<( ?S rdf:type ?B )>>}.`) -- the same missing `<<`/`>>` grammar
  production as `n3-star`, confirmed the same way.
- `n3c`: genuinely out of scope, not a grammar gap. Both of its query
  files (`forAllIn_query.n3`, `notIn_query.n3`) depend on `log:semantics`
  resolving a *remote* document by IRI (`?DOC log:semantics ?F` where
  `?DOC` is bound to a `https://eyereasoner.github.io/...` URL) before
  the derived `n3c:forAllIn`/`n3c:notIn` rules in `components.n3` can even
  run -- real network I/O, which the base plan (Phase 3, `log:` builtins)
  already documents as explicitly out of scope ("`log:semantics` limited
  to already-loaded graphs (no network I/O)"). There is no local-only
  variant of these two query files to fall back to.
- `n3gl`: parses successfully (no grammar gap -- `Subject`/`Object` both
  legitimately include `Formula`, so a quoted formula as the subject of a
  body/head triple pattern is valid syntax here) but is a genuine
  reasoning-engine limitation, verified by actually running
  `materialize()` on the real upstream `example.n3` (vendored temporarily
  to `lib/tests/n3_conformance/vendored/n3gl.n3` for the probe, then
  removed since the case does not produce a correct result). The file's
  first rule (`{?p a :SymetricRelation. ?s ?p ?o} => {?o ?p ?s}`) derives
  fine (`:bob :marriedTo :alice` and the resulting `:TEST :PASS 1`), but
  its second rule -- the one that actually exercises this directory's
  distinct mechanism, matching a *bound* quoted formula against a
  formula-shaped pattern in a body literal's subject position
  (`{?p a :SymetricRelation. {?s ?p ?o} ?p2 ?o2} => {{?o ?p ?s} ?p2
  ?o2}.`, meant to match the fact `{:alice :marriedTo :bob} :since
  "1999".` structurally and derive `:TEST :PASS 2` and `:TEST :PASS 3` --
  produces neither. This engine stores quoted formulas as an opaque
  encoded blob rather than unifying a formula pattern's internal
  variables against a previously-asserted formula's actual triple
  content, so the pattern silently fails to match anything. This is the
  same class of gap as `meta-interpretation`'s skip above (deep,
  content-level matching into a nested/quoted formula), not a
  domain-demo exclusion, and vendoring it would mean hand-writing the two
  missing PASS facts the engine does not actually derive, which the
  task's rules forbid.
- `n3patch`: genuinely unsupported on multiple independent axes, verified
  against the real `patch-rule.n3`/`patch.n3`. It uses a bare `true` as a
  rule's Head (`{ ... } => true.`), which this grammar's `Head` production
  (`"{" TP+ "}" | DenyHead("false")`) does not accept (same gap as `reif`
  above); it also depends on `log:semantics` resolving a *remote* IRI
  (`<https://.../patch.n3> log:semantics { ... }`, the same network-I/O
  exclusion as `n3c`) and on `log:callWithCleanup`/`log:becomes`, neither
  of which this engine implements. Any one of these three would already
  disqualify it.
- `n3plus1`: genuinely unsupported syntax on every feature the directory
  exists to demonstrate, verified against the real `in.n3`. It relies on:
  quad (4-term) statements (`:s :p :o _:g.` -- `TP`'s production is
  `Subject PredicateObjectList "."?`, a 3-term triple, with no room for a
  trailing graph term); blank-node-labeled named-graph blocks (`_:g { :s
  :p _:x. }` -- no such production exists; a bare `BlankNode` immediately
  followed by a `Formula` is not a valid top-level `document` item);
  RDF-star reification terms (`<< :d :e :f >>`, `<< :a :b _:x ~ :r >>` --
  the `~` origin-suffix variant of the same missing `<<`/`>>` gap as
  `n3-star`); and RDF-star annotation syntax (`:s :p :o {| :j :k |} .` --
  no `{|`/`|}` production anywhere in `n3.pest`). None of this parses.
- `proof-by-cases`: genuinely unsupported syntax, verified against both
  real upstream files (`example1.n3`, `example2.n3`). Both files' "proof by
  cases" rule passes a *list of nested rules* as the argument to
  `log:forAllIn` (e.g. `({ ?Y list:member { ?X a ?Z } } { { ?X a ?Z } =>
  { ?T :isProvenFor ?X } }) log:forAllIn ?SCOPE.` -- the second list
  element, `{ { ?X a ?Z } => { ?T :isProvenFor ?X } }`, is a quoted
  formula whose sole content is itself a full `=>` rule). This engine's
  grammar restricts both `Formula` (`"{" ~ (ForAll | ForSome | TP)* ~
  "}"`) and `ListItem` to plain triple patterns (`TP`), not a nested
  `rule` production, so a rule-as-list-element cannot parse. Confirmed:
  `Parser::parse_n3_document` errors at the nested `=>` in both files
  (`example1.n3` at `36:24`, `example2.n3` at `31:26`); the legacy
  line-splitting fallback then panics (`parser.rs:74`, `Option::unwrap()`
  on multi-line content) rather than producing a usable partial result.
  This is the same root "rule cannot appear where only a triple pattern
  is expected" gap as `bnode-scope`/`derived-rule` above, in a third
  position (list element rather than a rule's own Head).
- `proof-by-contradiction`: genuinely unsupported syntax, verified against
  both real upstream files. Both files' outer rule body quotes a formula
  passed to `log:satisfiable false` whose content is itself one or more
  full `=>` rules (e.g. `{ ?X a :Human. { ?X a :Human } => { ?X a
  :Mortal }. { ?X a :Mortal } => false. } log:satisfiable false.` --
  `example1.n3`) -- the same "rule nested inside a `Formula`, which this
  grammar only allows to contain `TP`" gap as `proof-by-cases` above, this
  time as the *subject* of a body literal rather than a list element.
  Confirmed: `Parser::parse_n3_document` errors at the first nested `=>`
  in both files (`example1.n3` at `12:26`, `example2.n3` at `7:5`), the
  legacy fallback then panics the same way. `log:satisfiable` itself is
  also not a registered builtin in `builtins::classify()` (a second,
  independent gap), moot given the grammar already rejects the input.
- `proof-by-contrapositive`: genuinely unsupported syntax on a different
  axis than the two directories above, verified against both real
  upstream files. Both files' rules use a *variable standing for an
  entire implication* -- `{ ?P => ?C. ?C => false } => { ?P => false }.`
  (`example1.n3`) and `{ { ?P => false } => false } => { true => ?P }.`
  (`example2.n3`) -- i.e. `=>` used as an ordinary infix triple-pattern
  predicate between two bound *variables* (not literal `{...}` formula
  terms), plus a bare `true` as a Head term. This grammar's `TP`
  production (`Subject ~ PredicateObjectList ~ "."?`) has no alternative
  admitting `=>`/`<=` as an infix operator inside an ordinary triple
  pattern (those tokens are reserved for the top-level `rule` production
  between two literal `{ }` formulas), and `Head` does not accept a bare
  `true` (same gap as `reif`/`n3patch` above). Confirmed:
  `Parser::parse_n3_document` errors at the `?P => ?C` body literal in
  both files (`example1.n3` at `11:7`, `example2.n3` at `5:25`).
- `proof-by-induction`: genuinely unsupported syntax, verified against the
  real upstream `example1.n3`. Its base case, `0 :sum 0.`, uses a bare
  numeric literal (`0`) as the *subject* of a triple -- this grammar's
  `Subject` production (`PathExpr | IriRef | Prefixed | Var | BlankNode |
  List | Formula | BNodeProps`) does not include a bare `Literal`, the
  identical gap already documented for `ill-formed-literals` above.
  Confirmed: `Parser::parse_n3_document` errors exactly at that line
  (`8:1`). The recursive case (a `<=` rule over `math:greaterThan`/
  `math:difference`/`math:sum`) would parse and materialize fine in
  isolation, but the base fact it depends on does not, so there is no
  honest way to vendor a truncated version without also dropping the
  induction's actual base case.
- `sequents`: genuinely unsupported syntax on the same "`=>` as an
  ordinary infix triple-pattern predicate between variables" axis as
  `proof-by-contrapositive`, verified against the real upstream
  `sequents.n3`. Every one of its seven resolution/rewriting rules uses a
  body literal of the exact shape `?A => ?B.` (e.g. the first rule's `?A
  => ?B. ?B list:select (?C ?D). ?C => ?E. ...`), which this grammar's
  `TP` production cannot parse for the reasons given above (reserved
  top-level-only `=>` token, not a `PredicateObjectList` predicate).
  Confirmed: `Parser::parse_n3_document` errors at the first such literal
  (`7:9`, `?A => ?B.` inside the "resolution A" rule body). The file also
  uses `($ $)` (an EYE-specific "anything" list-wildcard pattern) and a
  quoted-rule-as-consequent (`?E => { ?C => ($ $). }.` in "rewriting C"),
  each an independent reason this directory would still fail even if the
  `=>`-as-predicate gap were fixed.
- `path-discovery`: genuinely unsupported builtins, not a domain-demo
  exclusion. `path-discovery-algorithm.n3`'s two recursive backward `<=`
  rules for graph-path search depend on `list:notMember` and
  `list:firstRest` (`?visited list:notMember ?to.` /
  `?newVisited list:firstRest (?from ?visited).`). Neither IRI appears in
  `lib/src/builtins/list.rs` or `builtins::classify()` (confirmed by
  inspection: only `length`/`in`/`append`/`first`/`rest`/`last`/`member`/
  `memberAt`/`remove`/`sort`/`unique`/`reverse`/`iterate` are registered --
  `notMember` and `firstRest` are absent from both the constant list and the
  dispatch table). Empirically confirmed too: the real algorithm file
  parses without error (both rules produce valid `Rule`s), but running
  `TripleStore::materialize()` against it plus a single seed
  `nepo:hasOutboundRouteTo` fact derives **zero** triples -- the unknown
  builtin predicates in the rule bodies never match, so the rules can never
  fire. The directory's actual payload (`path-discovery-data.ttl`, 4.8 MB of
  real-world airline-route triples) would also need drastic truncation, but
  that's moot given the underlying builtins gap makes any truncated version
  produce nothing regardless.
- `ackermann`: genuinely unsupported syntax, verified against the real
  upstream `ackermann.n3`. Its x=0/x=1/x=2 base-case rules each guard on a
  literal EYE cut idiom, `true log:callWithCut true.`, as a body literal --
  a bare `true` (`BoolLiteral`) used as the *subject* of an ordinary triple
  pattern. This grammar's `Subject` production (`PathExpr | IriRef |
  Prefixed | Var | BlankNode | List | Formula | BNodeProps`) has no
  `BoolLiteral`/`Literal` alternative (the same "literal-as-subject" gap
  already documented for `ill-formed-literals`/`proof-by-induction`, here
  in a body-literal position rather than a fact or rule-base-case
  position). Confirmed: `Parser::parse_n3_document` errors exactly at that
  line (`19:5`, `expected TP`). `log:callWithCut` is also not a registered
  builtin in `builtins::classify()` (a second, independent gap), moot given
  the grammar already rejects the input. `ackermann-query.n3` (the goal
  file) does parse on its own, but with no working `ackermann.n3` rule set
  to run it against, there's nothing to honestly derive.
- `fibonacci`: genuinely unsupported syntax on two independent axes,
  verified against both real upstream files. `fibonacci.n3`'s recursive
  rule uses EYE's `!`-path sugar *as an element of a list term* --
  `((?X 1)!math:difference ?B (?A ?B)!math:sum)` -- but this grammar's
  `ListItem` production (`IriRef | Prefixed | Var | BlankNode | Literal |
  List | Formula | BNodeProps`) has no `PathExpr` alternative, unlike
  `Subject`/`Object` which both do; path sugar is valid at the top level of
  a triple pattern but not inside a list literal. Confirmed:
  `Parser::parse_n3_document` errors exactly at that list element (`8:56`,
  `expected ListItem`). Independently, the query file `fibonacciQ.n3` uses
  a bare numeric literal (`0`) as a triple's *subject* (`{0 :fibonacci
  ?Y} => {0 :fibonacci ?Y}.`), the same literal-as-subject gap as
  `ill-formed-literals`/`proof-by-induction`/`ackermann` above, confirmed
  at `3:2`. Either gap alone would disqualify this directory.
- `collatz`: same two independent gaps as `ackermann`/`fibonacci` above,
  verified against both real upstream files. `collatz.n3`'s recursive step
  rule uses the identical `true log:callWithCut true.` cut idiom (bare
  `true` as a body literal's Subject) at line 7, confirmed by
  `Parser::parse_n3_document` erroring at `7:5` (`expected TP`) -- the same
  gap, and also depends on the unregistered `log:callWithCut` builtin.
  Independently, `collatz-query.n3` uses a bare numeric literal (`1000`) as
  a triple's subject (`1000 log:repeat ?N0.`), confirmed erroring at `6:5`;
  `log:repeat` is also not a registered builtin in `builtins::classify()`,
  moot given the grammar already rejects it.
- `gcd-bezout-identity`: parses successfully in full (51 facts, 56 rules --
  no grammar gap; its `[ a :Answer; :gcd ?r0; ... ]` bnode-property-list
  heads and `math:product`/`math:difference`/`math:integerQuotient`/
  `math:absoluteValue` bodies are all within this grammar's supported
  subset) but is a genuine reasoning/builtin-registration gap, verified by
  actually running `TripleStore::materialize()` on the real, complete
  upstream file (all 6 harness cases, unmodified). The extended-Euclid
  "step" rule that must fire repeatedly to compute each case's gcd/Bezout
  coefficients guards on `?r1 math:notEqualTo 0.` -- but `math:notEqualTo`
  is absent from `builtins::classify()` (only `log:notEqualTo` is
  registered; confirmed by inspection of `lib/src/builtins/mod.rs`'s
  `classify()` match arms and `lib/src/builtins/math.rs`'s constants).
  With the guard unrecognized as a builtin, it is treated as an ordinary
  triple pattern requiring a literal `?r1 math:notEqualTo 0` fact already
  in the store -- never true -- so the step rule can never fire for any of
  the 6 cases. Confirmed empirically: materializing the real file derives
  exactly 98 triples, all either static vocabulary/harness facts or each
  case's single initial `:absA`/`:absB`/seed-`:Work` state -- zero
  `:answer`, `:check`, `:gcd`, or `:status` triples across all 6 cases,
  i.e. the directory's entire actual point (computing gcd/Bezout
  coefficients) never derives anything. Vendoring this would require
  hand-writing the gcd/Bezout results the engine does not actually derive,
  which the task's rules forbid; skipped and documented as a real,
  reproducible builtin-registration gap (distinct from the grammar-parse
  gaps above) rather than a domain-demo exclusion.
- `goldbach`: genuinely unsupported syntax on two independent axes, verified
  against both real upstream files (`goldbach.n3`, `goldbach-query.n3`).
  `goldbach.n3`'s seed fact `4 :goldbach (2 2).` uses a bare numeric literal
  (`4`) as a triple's *subject* -- the same literal-as-subject gap already
  documented for `ill-formed-literals`/`proof-by-induction`/`ackermann`/
  `collatz` (this grammar's `Subject` production has no `Literal`
  alternative). Confirmed: `Parser::parse_n3_document` errors exactly at
  that fact, `7:1`, `expected EOI, Prefix, Base, SparqlPrefix, SparqlBase,
  ForAll, ForSome, Keywords, TP, or rule`. Independently,
  `goldbach-query.n3` uses `36 log:repeat ?I.` -- another bare-numeric-
  literal-as-subject instance, confirmed erroring at `6:5`
  (`expected TP`); `log:repeat` is also not a registered builtin in
  `builtins::classify()`, moot given the grammar already rejects it.
  `goldbach.n3` additionally depends on the unregistered `log:callWithCut`
  cut idiom (`true log:callWithCut true.`, twice) -- a third, independent
  gap, moot given the first already blocks parsing.
- `kaprekar`: genuinely unsupported syntax on two independent axes,
  verified against both real upstream files. `kaprekar.n3`'s digit-to-
  number reconstruction rule uses EYE's `!`-path sugar *as an element of a
  list term* -- `((?A 1000)!math:product (?B 100)!math:product (?C
  10)!math:product ?D)` -- the same list-item-path-sugar gap already
  documented for `fibonacci`/`padovan` (this grammar's `ListItem`
  production has no `PathExpr` alternative). Confirmed:
  `Parser::parse_n3_document` errors exactly at that list element
  (`31:15`, `expected ListItem`). Independently, `kaprekar-query.n3` uses
  `10000 log:repeat ?I.` -- a bare-numeric-literal-as-subject instance,
  confirmed erroring at `5:5` (`expected TP`); `log:repeat` is also
  unregistered. `kaprekar.n3`'s main recursive rule additionally guards on
  `math:notEqualTo` (via `?A math:notEqualTo 0`), the same unregistered-
  builtin gap already documented for `gcd-bezout-identity` -- a third,
  independent gap, moot given the first already blocks parsing.
- `padovan`: genuinely unsupported syntax on two independent axes,
  verified against all three real upstream files (`padovan.n3`,
  `padovanA.n3`, `padovanQ.n3`). The recursive step rule uses the
  `!`-path-sugar-inside-a-list-item gap (`((?X 1)!math:difference ?B ?C
  (?A ?B)!math:sum)`), confirmed erroring at `9:59` (`expected ListItem`)
  -- the identical gap as `kaprekar`/`fibonacci` above. Independently,
  both `padovanA.n3` (the seed/expected-answer facts, e.g. `1 :padovan
  1 .`) and `padovanQ.n3` (the per-index query rules, e.g. `{1 :padovan
  ?Y} => {1 :padovan ?Y}.`) use a bare numeric literal as a triple's
  subject throughout, confirmed erroring at `3:1` and `3:2` respectively
  -- the same literal-as-subject gap as `goldbach`/`hanoi` below. Either
  gap alone would disqualify this directory; `padovan.n3`'s base cases
  (`{(0 ?A ?B ?C) :pad ?A} <= true.`) do use the grammar's supported
  `TrueBody` production for a bare `true` rule *body* (distinct from, and
  not blocked by, the separately-documented bare-`true`-as-*Head* gap) --
  confirmed by inspection of `n3.pest`'s `Body = ("{" ~ BodyLiteral+ ~
  "}") | TrueBody` / `TrueBody = "true"` productions -- so that part of
  the file is not actually a gap, but the two gaps above still are.
- `takeuchi`: the one directory in this batch with **no grammar gap at
  all** -- `tak.n3`, `takA.n3`, and `takQ.n3` all parse successfully
  (confirmed via `Parser::parse_n3_document`), including the base-case
  guard `{?X math:notGreaterThan ?Y} log:callWithCut true.`, which is
  syntactically valid here (a quoted `Formula` as the body literal's
  Subject, `log:callWithCut` as an ordinary prefixed-IRI predicate, `true`
  as a `BoolLiteral` Object -- all within this grammar's supported
  `Subject`/`Object` productions, unlike the bare-`true`-as-*Subject*
  idiom used by `ackermann`/`collatz`/`goldbach`). It was still not
  vendored: `log:callWithCut` is not a registered builtin in
  `builtins::classify()` (confirmed by inspection of
  `lib/src/builtins/mod.rs`), so this guard is treated as an ordinary
  triple pattern requiring a literal `{?X math:notGreaterThan ?Y}
  log:callWithCut true` fact already asserted in the store -- never true
  -- meaning the recursion's only base case never fires. Confirmed
  empirically by actually running `TripleStore::from`/materialize on the
  complete, real upstream file (`tak.n3` + `takA.n3`'s seed fact `(14 7 0)
  :tak 7 .` + `takQ.n3`'s query rule): the resulting store contains
  exactly 1 triple total -- the seed fact itself, unchanged -- zero
  additional `:tak` derivations, i.e. the directory's entire actual point
  (computing the Takeuchi function recursively) never derives anything.
  This is the same class of real, reproducible reasoning-engine limitation
  as `gcd-bezout-identity` (an unregistered builtin silently guard-blocks
  all recursion), not a grammar or domain-demo exclusion.
- `hanoi`: genuinely unsupported syntax, verified against both real
  upstream files. `hanoi.n3`'s query/answer-extraction rule, `{(3 :left
  :right :center) :moves ?M} => {3 :answer ?M}.`, uses a bare numeric
  literal (`3`) as the Head triple's subject -- the same literal-as-
  subject gap as `goldbach`/`padovan` above, here inside a rule's Head
  rather than a top-level fact. Confirmed: `Parser::parse_n3_document`
  errors exactly at that position, `18:42` (`expected TP`).
  Independently, `hanoiA.n3` (the expected-answer fact file) opens with
  `3 :answer (...).` -- another bare-numeric-literal-as-subject instance
  at the top level, confirmed erroring at `3:1`. `hanoi.n3`'s actual
  recursive `:moves` rule (the part that would exercise a genuinely new
  mechanism -- `list:append` composing sub-solutions) has no gap of its
  own and does use the supported `TrueBody` production for its base case
  (`{(1 ?X ?Y ?Z) :moves ((?X ?Y))} <= true.`), but the directory as a
  whole cannot be vendored without either fixing the literal-as-subject
  grammar gap (out of scope for a vendoring batch) or faking the
  `:answer` extraction step, which the task's rules forbid.
- `diamond-property`: genuine reasoning-engine limitation, not a grammar or
  domain-demo exclusion, verified by actually running `materialize()` on
  the real, complete upstream `diamond-property.n3` + `diamond-property-query.n3`.
  The DP-closure rule, `{?X :r ?Y. ?X :r ?Z} => {?Y :r ?U. ?Z :r ?U}.`,
  introduces `?U` only in the Head (a fresh existential witness per rule
  firing, per the EYE/N3 convention that head-only variables are
  skolemized). This engine's `substitute_head_with_bindings`
  (`lib/src/reasoner/substitution.rs`) has no skolemization step for
  head-only variables: when `?U` has no binding, `VarOrTerm::substitute_deep`
  leaves it unresolved and the triple is asserted anyway with the bare
  variable's name serialized as a literal-looking term (confirmed
  empirically: materializing the real file derives triples like `<dpe#b>
  <dpe#r> U` and `U <dpe#r> U`, where `U` is not a proper IRI/bnode/literal
  at all, just the leaked variable identifier). Because of this bug, the
  query goal `{:b :re ?X. :c :re ?X}` does end up satisfied in this
  engine's output (via the bogus shared term `U` accidentally standing in
  for both `:b :re` and `:c :re`), but only as an artifact of the
  skolemization bug, not a genuine diamond-property proof -- vendoring
  this would mean recording a result that "passes" for the wrong reason,
  which the task's rules do not permit presenting as a real derivation.
- `four-types-of-specification`: pure domain/business-logic demo (five
  government-benefit-eligibility scenarios -- student grants, bike-pass
  discounts, health copay aid, leisure vouchers, drought relief), each
  combining SHACL shape validation (a cross-cutting concern already
  covered by this crate's separate `shacl.rs` module, not the N3
  conformance suite's concern) with ordinary eligibility business rules
  that re-exercise mechanisms (rdfs typing, numeric comparisons, list
  membership) already covered by existing vendored/builtin cases. No
  distinct N3 language mechanism; excluded per the plan's documented
  domain-demo exclusion criterion (same class as `gdpr-compliance`,
  `dairy-energy-balance`, etc).
- `euq`: genuinely unsupported on both variants of the file, verified
  against both real upstream files. `euq.n3` (the file the directory's
  `test` script actually runs) declares its quantified variable as a URI,
  `@forAll <#X> .`, then uses `<#X>` as an ordinary triple-pattern term --
  this grammar's `ForAll` production (`n3.pest`) is `"@forAll" ~ Var ~
  ("," ~ Var)* ~ "."`, and `Var` requires a leading `?` (`"?" ~
  ASCII_ALPHA ~ ...`), so a bare IRI is not a valid `ForAll` argument at
  all. Confirmed: `Parser::parse_n3_document` errors exactly at that
  line (`expected Var`). The directory's alternate file, `euqA.n3`
  (EYE's own fallback rewrite for engines that don't support
  `@forAll`-over-URIs, substituting the `http://www.w3.org/2000/10/swap/var#`
  namespace's `var:qu_1` as a stand-in "variable"), *does* parse cleanly
  in this grammar (a plain prefixed IRI is ordinary `Subject` syntax), but
  this engine has no special-case handling of the `swap/var#` namespace
  (confirmed by inspection -- no `var#`/`qu_` handling anywhere in
  `lib/src/`), so `var:qu_1` is treated as an ordinary constant IRI, not a
  universally-quantified variable. Confirmed empirically: materializing
  the real, complete `euqA.n3` derives **zero** triples beyond the single
  input fact `:a :valid true.` -- the rule body `var:qu_1 :valid true.`
  never matches (the seed fact is about `:a`, not the constant
  `var:qu_1`), so `:all :is :satisfied.` (the case's entire point) never
  derives. Neither file honestly exercises the directory's mechanism in
  this engine.
- `qiana`: genuinely unsupported syntax on two independent axes, verified
  against the real upstream `qiana.n3`. Its second rule, `{ :Einstein
  :says ?phi } => ?phi.`, uses a bare variable as the rule's Head --
  this grammar's `Head` production (`"{" ~ TP+ ~ "}" | DenyHead`) has no
  bare-`Var` alternative (confirmed: `Parser::parse_n3_document` errors
  exactly there, `expected DenyHead`). Independently, the file's third
  statement, `:Einstein :says { { ?x a :glitter } => { ?x
  :notNecessarilyA :gold } }.`, embeds a full `=>` rule as the *content*
  of a quoted formula used as an ordinary assertion's object -- this
  grammar's `Formula` production (`"{" ~ (ForAll | ForSome | TP)* ~ "}"`)
  only admits plain triple patterns inside `{ }`, not a nested `rule`
  production, the same "rule nested where only a `TP` is allowed" gap
  already documented for `bnode-scope`/`derived-rule`/`proof-by-cases`
  above, here in a fourth position (a formula used as an ordinary triple's
  object, rather than a rule's Head/Body or a list element). Either gap
  alone would disqualify this directory, and together they block the
  file from parsing at all.
- `dcg`: genuinely unsupported builtins, not a grammar gap (the file
  parses -- `[] log:dcg "...", "...".` and `{(...) log:phrase (...)} =>
  {...}` are both valid `Subject`/predicate/`Object` shapes in this
  grammar). `log:dcg` (registering a set of quoted Prolog-DCG-style grammar
  rules as string literals) and `log:phrase` (parsing/generating a
  sentence against those DCG rules) are neither registered in
  `builtins::classify()` nor implementable without a full embedded
  Prolog-DCG interpreter -- confirmed by inspection of
  `lib/src/builtins/mod.rs`'s `classify()` match arms (no `dcg`/`phrase`
  entries under the `log:` namespace). This is a genuinely distinct
  mechanism (natural-language-style grammar parsing) this engine simply
  does not implement, not a domain-demo exclusion.
- `lingua`: genuinely out of scope -- not N3 syntax at all, verified
  against the real upstream `README.md` and its embedded examples. Every
  rule/query in this directory is written in RDF **TriG**, using
  named-graph blocks with a blank-node graph label as the top-level
  syntax (`_:bng_1 log:implies _:bng_2.` followed by `_:bng_1 { ... }` /
  `_:bng_2 { ... }` blocks, and `_:bng_3 log:impliesAnswer _:bng_3.` for
  queries) -- this grammar's `document` production (`n3.pest`) has no
  alternative admitting a bare blank-node label immediately followed by a
  `{ }` block as a top-level graph statement; the closest existing
  construct, `Formula`, is only valid in `Subject`/`Object`/`ListItem`
  position, never as its own top-level document item paired with a
  preceding blank-node graph-name subject. This is the same class of gap
  as `n3plus1`'s blank-node-labeled named-graph blocks (`_:g { ... }`)
  documented above, confirmed the same way (by grammar inspection, since
  the construct does not appear anywhere in `n3.pest`'s production list).
- `medic`: genuinely unsupported builtin, not a grammar gap (the file
  parses -- list-valued subjects like `(:Ann :Fever) fl:mu 0.8.` and
  nested body literals are both within this grammar's supported
  `Subject`/`BodyLiteral` shapes). The directory's single rule depends on
  `e:findall` (the `http://eulersharp.sourceforge.net/2003/03swap/log-rules#`
  namespace's aggregate-collection builtin, used twice in the rule body
  with a formula-valued generator argument, `?SCOPE e:findall (?EXCLUSION
  {...} ?LIST)`), which is not registered in `builtins::classify()`
  (confirmed by inspection -- only the `log:collectAllIn`/`log:forAllIn`
  reasoner-level equivalents are implemented, not the distinct `e:`
  namespace's `findall`). Without it, the rule's guard literals are
  treated as ordinary unmatched triple patterns and the rule never fires;
  vendoring would require either faking the result or implementing a new
  builtin, both out of scope for a vendoring batch.
