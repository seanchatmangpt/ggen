# P2P CLI Integration Architecture

## Command Flow Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        USER COMMAND LINE                                │
│  $ ggen marketplace p2p start --bootstrap /ip4/x.x.x.x/tcp/4001/...    │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    CLI LAYER (clap-noun-verb v3)                        │
│                   cli/src/cmds/marketplace.rs                           │
│                                                                         │
│  pub enum MarketplaceCmd {                                             │
│      Search(SearchArgs),                                               │
│      Install(InstallArgs),                                             │
│      P2p(P2PArgs),  ◄─── ✅ P2P INTEGRATION POINT                     │
│  }                                                                     │
│                                                                         │
│  impl MarketplaceArgs {                                                │
│      pub fn execute(&self) -> Result<()> {                            │
│          match &self.command {                                        │
│              MarketplaceCmd::P2p(args) => {                          │
│                  runtime::execute(                                    │
│                      execute_p2p_command(args.command.clone())       │
│                  )                                                    │
│              }                                                         │
│          }                                                             │
│      }                                                                 │
│  }                                                                     │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    RUNTIME BRIDGE (async/sync)                          │
│                      cli/src/runtime.rs                                 │
│                                                                         │
│  pub fn execute<F>(future: F) -> Result<()> {                         │
│      let runtime = tokio::runtime::Runtime::new()?;                   │
│      runtime.block_on(future)                                         │
│  }                                                                     │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    DOMAIN LAYER (business logic)                        │
│              cli/src/domain/marketplace/p2p.rs                          │
│                                                                         │
│  pub enum P2PCommand {                                                 │
│      Start(StartArgs),        ◄─── ggen marketplace p2p start         │
│      Publish(PublishArgs),    ◄─── ggen marketplace p2p publish       │
│      Search(SearchArgs),      ◄─── ggen marketplace p2p search        │
│      PeerList(PeerListArgs),  ◄─── ggen marketplace p2p peer-list    │
│      PeerInfo(PeerInfoArgs),  ◄─── ggen marketplace p2p peer-info    │
│      Bootstrap(BootstrapArgs),◄─── ggen marketplace p2p bootstrap     │
│      Status,                   ◄─── ggen marketplace p2p status        │
│  }                                                                     │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    FEATURE GATE CHECK                                   │
│                                                                         │
│  #[cfg(feature = "p2p")]                                               │
│  {                                                                     │
│      // Full P2P implementation                                       │
│      use ggen_marketplace::backend::p2p::P2PRegistry;                 │
│      let registry = P2PRegistry::new(config).await?;                 │
│  }                                                                     │
│                                                                         │
│  #[cfg(not(feature = "p2p"))]                                         │
│  {                                                                     │
│      Err(GgenError::feature_not_enabled("p2p", "..."))              │
│  }                                                                     │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    BACKEND LAYER (libp2p networking)                    │
│              ggen-marketplace/src/backend/p2p.rs                        │
│                                                                         │
│  🚧 STATUS: Backend has compilation issues (separate from CLI)        │
└─────────────────────────────────────────────────────────────────────────┘
```

## Key Design Decisions

### 1. Clap-Noun-Verb v3 Pattern
```bash
# Noun: marketplace
# Verb: p2p
# Sub-verbs: start, publish, search, peer-list, peer-info, bootstrap, status

ggen marketplace p2p <sub-verb> [args]
```

### 2. Async/Sync Bridge
- P2P operations are async (libp2p requirement)
- CLI execute() is sync (clap requirement)
- Solution: runtime::execute() bridges the gap

### 3. Feature Gating
- P2P is opt-in via `--features p2p`
- Graceful error when feature not enabled

## Files Modified

```
ggen/
├── cli/
│   ├── src/
│   │   ├── cmds/
│   │   │   └── marketplace.rs         ← ✅ P2P integrated
│   │   ├── domain/
│   │   │   └── marketplace/
│   │   │       ├── mod.rs              ← ✅ P2P exports
│   │   │       └── p2p.rs              ← ✅ P2P commands
│   │   └── runtime.rs                  ← Async/sync bridge
│   └── Cargo.toml                      ← Feature: p2p
```

## Success Criteria ✅

- [x] P2P commands accessible via `ggen marketplace p2p <verb>`
- [x] Feature-gated with `#[cfg(feature = "p2p")]`
- [x] All subcommands wired to domain layer
- [x] Help text generated automatically by clap
- [x] Follows existing marketplace command patterns
- [x] Proper error handling
- [x] Documentation complete

**Status**: CLI integration 100% complete ✅
