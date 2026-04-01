# ggen-dspy — Crate Audit

**Path:** `crates/ggen-dspy/`
**Lines:** 10+ modules, ~317 test markers
**Role:** DSPy-inspired LLM agent framework (predictors, optimizers, modules)

---

## STUBS

| File:Line | Function | Returns | Should Do |
|-----------|----------|---------|-----------|
| `optimizers/bootstrap.rs:28` | `BootstrapFewShot::compile()` | `Ok(module)` unchanged | Auto-select demonstration examples for few-shot prompting |
| `optimizers/bootstrap.rs:7` | `config` field | `#[allow(dead_code)]` — stored, unused | |
| `optimizers/mipro.rs:28` | `MiproOptimizer::compile()` | `Ok(module)` unchanged | Multi-prompt instruction proposal optimization |
| `optimizers/mipro.rs:7` | `config` field | `#[allow(dead_code)]` — stored, unused | |
| `modules/program_of_thought.rs:168` | Non-JS languages | `Err("Code generation not implemented for {:?}", language)` | Python, Rust, other language code generation |

---

## DEAD CODE

| File:Line | Item | Status |
|-----------|------|--------|
| `modules/react.rs:329` | Unknown | `#[allow(dead_code)]` |
| `adapters.rs:79` | Unknown | `#[allow(dead_code)]` |
| `predictors/a2a_predictor.rs:433,811` | Fields | `#[allow(dead_code)]` |
| `config/cache.rs:97` | Unknown | `#[allow(dead_code)]` |

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | BootstrapFewShot optimizer: implement demo selection algorithm | P1 |
| **FIX** | MIPRO optimizer: implement instruction optimization | P1 |
| **FIX** | Program of Thought: add Python/Rust code generation | P2 |
| **DELETE** | Dead `config` fields in both optimizers | P3 |
| **DELETE** | Dead code in react, adapters, a2a_predictor | P3 |
