<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Accepted Security Advisories](#accepted-security-advisories)
  - [Vulnerabilities (transitive, test-only via `testcontainers`)](#vulnerabilities-transitive-test-only-via-testcontainers)
    - [RUSTSEC-2026-0113 — astral-tokio-tar `unpack_in` symlink chmod](#rustsec-2026-0113--astral-tokio-tar-unpack_in-symlink-chmod)
    - [RUSTSEC-2026-0066 — astral-tokio-tar PAX extensions validation](#rustsec-2026-0066--astral-tokio-tar-pax-extensions-validation)
    - [RUSTSEC-2026-0112 — astral-tokio-tar PAX Header Desynchronization](#rustsec-2026-0112--astral-tokio-tar-pax-header-desynchronization)
  - [Warnings (unmaintained transitive deps)](#warnings-unmaintained-transitive-deps)
    - [RUSTSEC-2025-0052 — async-std discontinued](#rustsec-2025-0052--async-std-discontinued)
    - [RUSTSEC-2024-0375 — atty unmaintained](#rustsec-2024-0375--atty-unmaintained)
    - [RUSTSEC-2021-0145 — atty unsound (potential unaligned read on Windows)](#rustsec-2021-0145--atty-unsound-potential-unaligned-read-on-windows)
    - [RUSTSEC-2024-0384 — instant unmaintained](#rustsec-2024-0384--instant-unmaintained)
    - [RUSTSEC-2025-0119 — number_prefix unmaintained](#rustsec-2025-0119--number_prefix-unmaintained)
    - [RUSTSEC-2024-0436 — paste unmaintained](#rustsec-2024-0436--paste-unmaintained)
    - [RUSTSEC-2024-0370 — proc-macro-error unmaintained](#rustsec-2024-0370--proc-macro-error-unmaintained)
    - [RUSTSEC-2025-0134 — rustls-pemfile unmaintained](#rustsec-2025-0134--rustls-pemfile-unmaintained)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Accepted Security Advisories

This file records security advisories that `cargo audit` flags but which
have been accepted as known-and-mitigated for the v26.5.19 release. Each
acceptance includes the provenance chain showing the advisory's reach,
the justification, and a planned-action-or-revisit-condition.

Configured ignores live in `scripts/run-audit.sh` as `--ignore <ID>` flags
passed to `cargo audit`. To revisit, remove the ignore and let `cargo
audit` fail loudly.

## Vulnerabilities (transitive, test-only via `testcontainers`)

### RUSTSEC-2026-0113 — astral-tokio-tar `unpack_in` symlink chmod

- **Provenance:** `astral-tokio-tar 0.5.6` ← `testcontainers 0.25.2` ←
  `ggen-core` (dev-deps), `testcontainers-modules` (dev-deps).
- **Reach:** Test infrastructure only. Not in the released `ggen` binary.
- **Justification:** `testcontainers` pins its `astral-tokio-tar`
  transitively; upstream upgrade to `>=0.6.1` requires a `testcontainers`
  bump. Accepted for v26.5.19 since the affected code path is only
  reached during local integration tests.
- **Revisit:** When `testcontainers` releases a version that uses
  `astral-tokio-tar >=0.6.1`.

### RUSTSEC-2026-0066 — astral-tokio-tar PAX extensions validation

- **Provenance:** Same transitive chain as above.
- **Reach:** Test infrastructure only.
- **Justification:** Same as RUSTSEC-2026-0113.
- **Revisit:** Same as RUSTSEC-2026-0113.

### RUSTSEC-2026-0112 — astral-tokio-tar PAX Header Desynchronization

- **Provenance:** Same transitive chain as above.
- **Reach:** Test infrastructure only.
- **Justification:** Same as RUSTSEC-2026-0113.
- **Revisit:** Same as RUSTSEC-2026-0113.

## Warnings (unmaintained transitive deps)

These crates are marked unmaintained but functionally work. None has a
known CVE; the advisory is preventive.

### RUSTSEC-2025-0052 — async-std discontinued

- **Provenance:** Transitive via `oxigraph`-adjacent deps.
- **Justification:** No CVE; replacement story is not yet stable across
  the ecosystem. Accept until upstream switches.

### RUSTSEC-2024-0375 — atty unmaintained
### RUSTSEC-2021-0145 — atty unsound (potential unaligned read on Windows)

- **Provenance:** `atty 0.2.14` ← `clap-noun-verb 5.6.2` ← `ggen-cli-lib`.
- **Reach:** Compiled into the released binary, but `atty` is invoked
  only for TTY detection at startup; the unaligned-read path is reachable
  only on uncommon x86 Windows configurations and reads from a stack
  position that is, in practice, always aligned.
- **Justification:** Accept until `clap-noun-verb` drops the `atty`
  dependency or upstreams a replacement.
- **Revisit:** Watch `clap-noun-verb` releases for a successor to `atty`
  (e.g., `is-terminal`).

### RUSTSEC-2024-0384 — instant unmaintained

- **Provenance:** Transitive via several deps.
- **Justification:** No CVE; functional.

### RUSTSEC-2025-0119 — number_prefix unmaintained

- **Provenance:** Transitive.
- **Justification:** No CVE; trivial crate.

### RUSTSEC-2024-0436 — paste unmaintained

- **Provenance:** Transitive (macro helper).
- **Justification:** No CVE; macro-only.

### RUSTSEC-2024-0370 — proc-macro-error unmaintained

- **Provenance:** Transitive (macro helper).
- **Justification:** No CVE; macro-only.

### RUSTSEC-2025-0134 — rustls-pemfile unmaintained

- **Provenance:** `rustls-pemfile 2.2.0` ← `bollard 0.19.4` ←
  `testcontainers 0.25.2` (dev-dep chain).
- **Reach:** Test infrastructure only.
- **Justification:** Not in released binary.
