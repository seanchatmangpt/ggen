# N3 Conformance Tests

This directory contains manifest-driven N3 conformance test cases for this
crate's N3 rule engine (parser + forward-chaining reasoner + built-in
predicates).

There is no official W3C Recommendation test suite for N3, so this suite is a
mix of:

- **`vendored/`** -- 19 active cases lifted (with light adaptation, documented
  in each file's header comment) from the
  [EYE reasoner](https://github.com/eyereasoner/eye)'s own public
  `reasoning/` test corpus: `socrates`, `path`, `dog`, `snaf`,
  `deep-taxonomy` (truncated from a 10000-level chain to 3 levels),
  `nixon-diamond`, `backward` (`backward_math`), `good-cobbler`, `peano`
  (two cases: a ground goal and a variable-goal solve), `skos`, `filter`,
  `basic-monadic` (truncated to a 10-node cycle), `edt` (truncated to a
  3-level chain), `entail`, `defeasable-logic`, `backward-more`, `restpath`,
  and `graph`. A large number of other candidate `reasoning/` directories
  were evaluated and deliberately **not** vendored -- see
  [`SKIPPED.md`](./SKIPPED.md) below.
- **`syntax/`** and **`builtins/`** -- hand-authored cases covering this
  grammar's syntax surface (multi-triple heads, literals, blank-node
  property lists, blank nodes, RDF lists, `@forAll`/`@forSome`, quoted
  graphs, `@keywords`, `=`/`<=>`/inverse sugar, backward implication `<=`,
  `@base`/relative-IRI resolution and SPARQL-style `PREFIX`/`BASE`, and path
  syntax `!`/`^`) and its N3 built-in predicates across `log:`, `math:`,
  `list:`, `string:`, `func:`, `time:`, and `crypto:` namespaces.

## Folder Structure

```text
lib/tests/n3_conformance/
├── README.md               # This file
├── SKIPPED.md               # Deliberately-excluded EYE reasoning/ directories, with reasons
├── manifest.json           # JSON index of all conformance test cases
├── main.rs                 # The manifest-driven test runner
├── syntax/                 # N3 syntax and structure test cases (12 active)
│   ├── multi_head.n3 / multi_head_result.n3
│   ├── literals.n3 / literals_result.n3
│   └── ...
├── builtins/               # Built-ins (log:, math:, list:, string:, func:, time:, crypto:) (70 cases, 1 ignored)
│   ├── log_equal_to.n3 / log_equal_to_result.n3
│   ├── math_sum.n3 / math_sum_result.n3
│   └── ...
└── vendored/                # Adapted EYE reasoner test cases (19 active)
    ├── socrates.n3 / socrates_result.n3
    ├── path.n3 / path_result.n3
    ├── dog.n3 / dog_result.n3
    ├── snaf.n3 / snaf_result.n3
    ├── deep_taxonomy.n3 / deep_taxonomy_result.n3
    ├── nixon_diamond.n3 / nixon_diamond_result.n3
    ├── backward_math.n3 / backward_math_result.n3
    ├── good_cobbler.n3 / good_cobbler_result.n3
    ├── peano.n3 / peano_result.n3
    ├── skos.n3 / skos_result.n3
    ├── filter.n3 / filter_result.n3
    ├── basic_monadic.n3 / basic_monadic_result.n3
    ├── edt.n3 / edt_result.n3
    ├── entail.n3 / entail_result.n3
    ├── defeasable_logic.n3 / defeasable_logic_result.n3
    ├── backward_more.n3 / backward_more_result.n3
    ├── restpath.n3 / restpath_result.n3
    └── graph.n3 / graph_result.n3
```

Each case is an `(input, expected)` pair of `.n3` files: `input` is a
complete N3 document (facts + rules), and `expected` is a plain N3 fact list
naming the triples that must appear among the derived (materialized) facts.

## Running Conformance Tests

`main.rs`'s `test_n3_conformance` test:
1. Parses `manifest.json`.
2. For each test case with status `active`:
   - Loads the `input` N3 file via `TripleStore::from` (facts + rules).
   - Runs `store.materialize()` to compute the forward-chaining fixpoint.
   - Decodes every inferred triple to its `"S P O."` string form.
   - Asserts every triple listed in `expected` is present among them.

This is a **subset/entailment check**, not exact-equality: a forward-chaining
fixpoint often derives more than any one hand-picked expectation captures,
and some of the vendored EYE cases were themselves checked by EYE against
only a narrow, non-generic query rather than the full closure -- see
`manifest.json`'s `runner_semantics` field and each case's own header
comments for the reasoning behind its specific `expected` file.

Running just this suite:

```sh
cargo test -p roxi --test n3_conformance
```

Regenerating the pass-rate manifest at
`docs/jira/26.7.4/manifests/n3_manifest.md` happens as a side effect of
running the test (see `main.rs`).

## Deliberately skipped EYE directories

Beyond the 19 vendored cases above, dozens of other `reasoning/<dir>`
candidates from the upstream EYE corpus were evaluated and *not* vendored --
some because they are pure domain/business-logic demos that only
re-exercise already-covered mechanisms (`n-queens`, `dijkstra`,
`dining-philosophers`, `gdpr-compliance`, and similar "-worlds"/policy/
simulation directories), and many others because they were verified,
real-content-in-hand, to depend on syntax or builtins this engine's grammar
or reasoner genuinely does not support (e.g. RDF-star quoted-triple terms,
`{| |}` annotations, EYE's `true <=`/bare-`true`-as-Head cut idioms,
literal-as-subject triples, `!`-path sugar inside a list element, or
unregistered builtins like `log:callWithCut`, `math:notEqualTo`,
`list:notMember`).

See [`SKIPPED.md`](./SKIPPED.md) for the full, per-directory list: each
entry records the concrete, engine-verified reason (parse error with
line/column, or an empirical `materialize()` run showing zero relevant
derivations) rather than a guess, so every exclusion is auditable.
