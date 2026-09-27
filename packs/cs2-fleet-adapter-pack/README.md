# CS2 Fleet Adapter Pack

This pack turns RDF consumer bindings into deterministic adapter surfaces. The ontology is the source; generated adapters are projections.

The pack separates subject identity from consumer-specific binding identity. A fleet can therefore share one exact subject while manufacturing language-, transport-, and namespace-specific adapters without duplicating the subject declaration by hand.

Surfaces included: Elixir, Rust, JSON, SQL upsert migration, OpenAPI 3.1, and Protocol Buffers. The example fixture binds xaas, gymact, and semantic-jira to RFC-CS2-001.

Tests under the example are authored fixtures only; maximum-code runs do not execute them.
