<!--
House convention: an unchecked box is more useful than a falsely checked one.
Check only what you actually verified. Leave the rest unchecked and explain.
-->

## Summary

<!-- One or two sentences: what does this change and why. -->

## Type of change

- [ ] Bug fix (non-breaking change that fixes an issue)
- [ ] New feature (non-breaking addition)
- [ ] Breaking change (changes existing API or CLI surface)
- [ ] Refactor (no behavior change, no API change)
- [ ] Documentation / comments only
- [ ] Chore (dependency bump, toolchain update, CI)

## Test plan

<!-- How did you verify this change? List the steps or test names. -->

- [ ] `just ci` passes locally (fmt-check, clippy `-D warnings`, test, deny, typos)
- [ ] New/changed behavior is covered by tests
- [ ] Public API changes are documented (rustdoc + CHANGELOG entry under `## [Unreleased]`)
- [ ] No new `unwrap`/`expect`/`panic` in library code paths

## What I did NOT verify

<!-- Be honest. e.g. "cross-compile targets", "the otel feature path", "Windows". -->

## Related issues

<!-- Closes #<issue>, Refs #<issue>, or "None" -->

## Notes for reviewers

<!-- Anything subtle, follow-ups, or deliberate trade-offs. -->
