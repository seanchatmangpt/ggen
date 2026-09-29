# Portable consequence reference projection

This is a compiler specimen, not a protocol owner. `ontology.ttl` is projected by ggen into a small Rust implementation of the `portable-consequence/1` admission relation. The generated file lives under `target/` and is deliberately disposable.

Acceptance:

```bash
ggen sync run --manifest examples/portable-consequence-reference/ggen.toml
rustc --test examples/portable-consequence-reference/target/portable-consequence/protocol.rs -o /tmp/portable-consequence-test
/tmp/portable-consequence-test
```

The crown is cross-implementation conformance against neutral vectors, not successful generation by itself.
