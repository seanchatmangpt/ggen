# W3C shexTest Vendored Subset

This directory contains a hand-converted, vendored slice of the official
[shexTest](https://github.com/shexSpec/shexTest) validation conformance suite
(`validation/manifest.jsonld` and its referenced `schemas/*.shex` and
`validation/*.ttl` files), copyright W3C.

## Why hand-converted, not verbatim

The official suite's schemas are written in **ShExC** (compact syntax).
`praxis_graphlaw::shex` (natively implemented in `lib/src/shex_native.rs`, no external
ShEx crate) only accepts **ShExJ** (the JSON syntax for ShEx schemas) — see
`validate_shex_native`, which calls `serde_json::from_str::<Schema>(...)`
against this crate's own minimal ShExJ AST. Some newer vendored cases use the
upstream repo's own pre-compiled `schemas/*.json` ShExJ files directly
(verbatim, no hand-translation needed) rather than converting from `.shex`.

ShExC and ShExJ are two surface syntaxes for the same abstract schema; each
`cases/<name>/schema.json` here is a manual, field-by-field translation of the
corresponding official `.shex` file into ShExJ. Each case's `meta.json`
records the original ShExC source (`source_shexc`), the original schema/data
file paths in the upstream repo (`source_schema`, `source_data`), and the
official test's own `comment` field, so the translation can be checked against
the upstream test by hand.

A few data files used relative IRIs together with a `PREFIX :
<http://a.example/>` declaration (e.g. `p1.ttl`, `p1p2p3.ttl`); those have
been expanded to fully-qualified IRIs in the vendored `data.ttl` copies. This
is noted in each affected case's `source_data` field. No other semantic
changes were made; triples and expected outcomes are otherwise verbatim.

## Coverage

This is a representative slice (see `w3c_runner.rs` for the full list),
covering: empty shapes, TripleConstraint cardinality, OneOf, EachOf, ShapeAnd,
ShapeOr, ShapeNot, CLOSED shapes, EXTRA, NodeConstraint nodeKind (iri/bnode),
value sets (plain IRI and IriStem), string facets (length), IRI pattern
facets, language-tagged literal value sets, and blank-node focus/object
nodes. It is **not** the full ~1180-test suite — the vast majority of that
suite is written in ShExC and is out of scope for this adapter-focused vendor
pass (see the top of `w3c_runner.rs` for the itemized skip rationale).

## License

Per the upstream repository's `LICENSE`:

> Permission to copy, modify, and distribute this work, with or without
> modification, for any purpose and without fee or royalty is hereby granted,
> provided that you include the following on ALL copies of the work or
> portions thereof, including modifications: The full text of this NOTICE in
> a location viewable to users of the redistributed or derivative work... The
> name and trademarks of copyright holders may NOT be used in advertising or
> publicity pertaining to the work without specific, written prior
> permission.

This software or document includes material copied from or derived from the
shexTest validation test suite (https://github.com/shexSpec/shexTest).
Copyright © W3C (MIT, ERCIM, Keio, Beihang). See
https://github.com/shexSpec/shexTest/blob/master/LICENSE for the full text.
