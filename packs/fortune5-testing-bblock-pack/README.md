# fortune5-testing-bblock-pack

This pack manufactures the executable `testing` bblock retained by `ggen bblock`. It creates nine independent suite entrypoints rather than collapsing all evidence into one undifferentiated test command:

- protocol/unit;
- property/fuzz;
- stdio plus HTTP integration;
- black-box CLI E2E;
- security;
- chaos;
- stress;
- benchmark;
- replay.

`verify-all.sh` executes the full ladder, writes `ggen.testing.verifier-report.v1`, chains every suite receipt with real BLAKE3, normalizes evidence paths relative to the report, and immediately replays the portable report. A suite is `ALIVE` only after observed execution. Missing tools are `BLOCKED`; command, determinism, integrity, security, or performance failures are `BUILD_BROKEN`.

The pack performs no cloud actuation. Its HTTP boundary is loopback-only and its construction surface is repository-local. Provider-specific packs admitted by the parent catalog supply downstream live-cloud fixtures without entering the generic bblock compiler.
