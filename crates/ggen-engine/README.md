# ggen

SPARQL-in-Tera code generation

> Part of the [`seanchatmangpt`](https://github.com/seanchatmangpt) Rust house
> style — scaffolded from
> [`praxis`](https://github.com/seanchatmangpt/praxis).

## Quick start

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
just            # list tasks
just ci         # fmt-check + clippy -D warnings + test + deny + typos
```

## Build & test

| Task    | Command          |
| ------- | ---------------- |
| Format  | `just fmt`       |
| Lint    | `just lint`      |
| Test    | `just test`      |
| Docs    | `just doc`       |
| Full CI | `just ci`        |

Toolchain is pinned to stable `1.82.0` via `rust-toolchain.toml`; MSRV is `1.82`.
Versioning is CalVer (`YY.M.patch`).

## License

Licensed under either of [MIT](LICENSE-MIT) or [Apache-2.0](LICENSE-APACHE) at
your option.
