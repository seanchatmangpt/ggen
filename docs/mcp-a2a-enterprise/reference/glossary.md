# Glossary

## A2A

Agent-to-agent coordination layer used to orchestrate multi-role workflows and handoffs.

## MCP

Model Context Protocol surface used to expose ggen capabilities as tool calls, resources, and prompts.

## JTBD

Jobs-to-be-done. In this context, the practical delivery goals ggen is intended to complete.

## Canonical ontology

Shared semantic model used as the source of truth for generation and validation behavior.

## Quality gate

Mandatory validation checkpoint that must pass before proceeding (for example, schema, dependencies, query/template validity).

## Manifest

`ggen.toml` project contract describing ontology source, generation rules, and output settings.

## Receipt

Execution evidence artifact that captures what generation/validation operation occurred and with what outcome.

## Import cycle

A circular ontology dependency chain where file A imports B and B (directly or transitively) imports A.

## Deterministic output

Property that the same inputs produce identical outputs across runs.

## Handoff contract

Structured payload exchanged between agent roles to transfer ownership while preserving assumptions and status.
