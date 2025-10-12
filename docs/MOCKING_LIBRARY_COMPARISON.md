# Rust Mocking Libraries - Quick Comparison Matrix

## ✅ Recommendation: **mockall**

Best fit for London School TDD in ggen lifecycle system.

---

## Feature Comparison

### Core Capabilities

| Feature | mockall | mockito | faux | double |
|---------|---------|---------|------|--------|
| **Trait Mocking** | ✅ Excellent | ❌ HTTP only | ❌ No | ⚠️ Limited |
| **Struct Mocking** | ✅ Yes | ❌ HTTP only | ✅ Primary | ❌ No |
| **Function Mocking** | ✅ Via traits | ❌ HTTP only | ⚠️ Limited | ❌ No |
| **Async Support** | ✅ Full | ✅ Full | ✅ Yes | ⚠️ Limited |

### Interaction Verification (Key for London TDD)

| Feature | mockall | mockito | faux | double |
|---------|---------|---------|------|--------|
| **Call Count Verification** | ✅ .times(n) | ✅ HTTP calls | ✅ Basic | ⚠️ Basic |
| **Argument Matching** | ✅ Rich matchers | ✅ HTTP matchers | ⚠️ Limited | ❌ No |
| **Call Order Verification** | ✅ Sequence | ❌ No | ⚠️ Limited | ❌ No |
| **Never Called** | ✅ .never() | ⚠️ HTTP only | ⚠️ Manual | ❌ No |
| **Custom Predicates** | ✅ .withf() | ✅ HTTP only | ⚠️ Limited | ❌ No |

### Return Value Control

| Feature | mockall | mockito | faux | double |
|---------|---------|---------|------|--------|
| **Fixed Returns** | ✅ .returning() | ✅ HTTP bodies | ✅ .then_return() | ✅ Yes |
| **Dynamic Returns** | ✅ Closures | ✅ Closures | ✅ Closures | ⚠️ Limited |
| **Error Returns** | ✅ Result<T, E> | ✅ HTTP status | ✅ Result<T, E> | ⚠️ Limited |
| **Stateful Returns** | ✅ Full | ⚠️ Limited | ✅ Yes | ⚠️ Limited |
| **Return Once** | ✅ .return_once() | ❌ N/A | ⚠️ Manual | ❌ No |

### Ease of Use

| Feature | mockall | mockito | faux | double |
|---------|---------|---------|------|--------|
| **Setup Complexity** | ⚠️ Moderate | ✅ Simple | ⚠️ Moderate | ⚠️ Complex |
| **Test Verbosity** | ⚠️ Moderate | ✅ Concise | ✅ Concise | ⚠️ Verbose |
| **Error Messages** | ✅ Clear | ✅ Clear | ⚠️ Basic | ⚠️ Unclear |
| **Documentation** | ✅ Excellent | ✅ Good | ⚠️ Limited | ⚠️ Minimal |
| **Examples** | ✅ Many | ✅ Many | ⚠️ Few | ⚠️ Few |

### Project Health

| Feature | mockall | mockito | faux | double |
|---------|---------|---------|------|--------|
| **Latest Version** | 0.13.1 | 1.7.0 | 0.1.13 | 0.2.4 |
| **Last Updated** | 2024 | 2024 | 2021 | 2018 |
| **Active Maintenance** | ✅ Yes | ⚠️ Stable | ❌ Inactive | ❌ Archived |
| **Community Size** | ✅ Large | ✅ Large | ⚠️ Small | ⚠️ Tiny |
| **Monthly Downloads** | ~500K | ~300K | ~50K | <10K |

---

## Why mockall for ggen?

### ✅ Perfect Fit

1. **Trait-heavy system** - Lifecycle uses many traits
2. **Interaction-based** - London TDD verifies collaborations
3. **Complex sequences** - Hooks and pipelines need order verification
4. **Rich matchers** - Command arguments need flexible matching
5. **Active maintenance** - Won't become abandoned
6. **Best documentation** - Easy to onboard team

### ❌ Why Not Others?

**mockito:**
- HTTP mocking only
- Wrong domain for lifecycle system
- Can't mock command execution or file I/O

**faux:**
- No trait support (lifecycle is trait-based)
- Limited interaction verification
- Inactive project (last update 2021)
- Small community

**double:**
- Project archived (no maintenance)
- No trait mocking
- Minimal features
- Security concerns with unmaintained deps

---

## Quick Start

### Installation

```toml
[dev-dependencies]
mockall = "0.13.1"
```

### Basic Usage

```rust
use mockall::automock;

#[automock]
trait CommandExecutor {
    fn execute(&self, cmd: &str) -> Result<()>;
}

#[test]
fn test_example() {
    let mut mock = MockCommandExecutor::new();

    mock.expect_execute()
        .with(eq("npm test"))
        .times(1)
        .returning(|_| Ok(()));

    mock.execute("npm test").unwrap();
}
```

---

## Decision Matrix

| Requirement | mockall | mockito | faux | double |
|-------------|---------|---------|------|--------|
| Trait mocking for CommandExecutor | ✅ | ❌ | ❌ | ⚠️ |
| Verify command call order | ✅ | ❌ | ⚠️ | ❌ |
| Test hook execution sequence | ✅ | ❌ | ⚠️ | ❌ |
| Match command arguments | ✅ | ❌ | ⚠️ | ❌ |
| Mock state persistence | ✅ | ❌ | ⚠️ | ⚠️ |
| Active maintenance | ✅ | ⚠️ | ❌ | ❌ |
| Good documentation | ✅ | ✅ | ⚠️ | ❌ |
| **Total Score** | **7/7** | **1/7** | **1/7** | **0/7** |

---

## Score Legend

- ✅ Excellent/Full support
- ⚠️ Partial/Limited support
- ❌ No support/Not applicable

---

## Next Steps

1. ✅ **Choose mockall** (this decision)
2. 📖 Read RUST_MOCKING_LIBRARIES_RESEARCH.md (detailed analysis)
3. 📖 Read MOCKALL_QUICK_START.md (practical guide)
4. 💻 Add mockall to ggen-core/Cargo.toml
5. 💻 Create lifecycle/traits.rs
6. 💻 Write first London School TDD test
7. 🚀 Refactor lifecycle system with TDD

---

**Research Date:** 2025-10-11  
**Decision:** Use mockall for London School TDD  
**Confidence:** High ✅
