# MCP + Rig Integration - Implementation Roadmap

## Overview

This roadmap outlines the step-by-step implementation of a comprehensive ggen example for MCP + Rig integration, based on the official Rust SDK example architecture.

## Phase 1: Template Development (Week 1)

### 1.1 Core Module Templates

**Objective:** Create foundational templates for all core modules

- [ ] **main-rs.tmpl** - Main application template
  - Configuration loading
  - LLM client initialization
  - MCP manager setup
  - Tool embedding and vector store
  - Chat agent construction

- [ ] **config-rs.tmpl** - Configuration module
  - TOML loading
  - API key management
  - Provider configuration

- [ ] **config-mcp-rs.tmpl** - MCP configuration submodule
  - Server configuration types
  - Transport variants (stdio, sse, streamable)
  - Manager creation logic

- [ ] **mcp-adaptor-rs.tmpl** - MCP adaptor module
  - McpToolAdaptor implementation
  - RigTool trait implementation
  - ToolEmbeddingDyn trait implementation
  - McpManager coordination

- [ ] **chat-rs.tmpl** - Chat interface module
  - CLI chatbot implementation
  - Streaming response handling
  - Tool call visualization
  - Error handling

### 1.2 Supporting Templates

- [ ] **cargo-toml.tmpl** - Cargo configuration
  - Dependencies with version management
  - Feature flags
  - Build configuration

- [ ] **config-toml.tmpl** - Runtime configuration
  - MCP server definitions
  - API key placeholders
  - Provider settings

- [ ] **lib-rs.tmpl** - Library entry point (optional)
  - Re-exports
  - Common utilities

### 1.3 Template Variables Schema

```yaml
# schemas/project-config.yaml
project:
  name: string (required)
  version: string (default: "0.1.0")
  authors: string[] (required)

llm_providers:
  deepseek:
    enabled: boolean (default: true)
    model: string (default: "deepseek-chat")
    api_key: string (optional)
  cohere:
    enabled: boolean (default: true)
    embedding_model: string (default: "embed-english-v3.0")
    api_key: string (optional)
  openai:
    enabled: boolean (default: false)
    model: string (default: "gpt-4o")
    api_key: string (optional)

chat:
  default_model: string (default: "gpt-4o")
  max_dynamic_tools: integer (default: 5)
  enable_streaming: boolean (default: true)
  enable_colored_output: boolean (default: true)

logging:
  level: string (default: "info")
  rotation: string (default: "daily")
  directory: string (default: "logs")

mcp_servers: McpServer[]

McpServer:
  name: string (required)
  protocol: enum[stdio, sse, streamable] (required)
  # stdio specific
  command: string (conditional)
  args: string[] (optional)
  # sse/streamable specific
  url: string (conditional)
  # common
  envs: map[string, string] (optional)
```

## Phase 2: Example Configurations (Week 2)

### 2.1 Pre-built Examples

- [ ] **examples/filesystem-server.toml**
  ```toml
  [[mcp.server]]
  name = "filesystem"
  protocol = "stdio"
  command = "npx"
  args = ["-y", "@modelcontextprotocol/server-filesystem", "./allowed-files"]
  ```

- [ ] **examples/api-server.toml**
  ```toml
  [[mcp.server]]
  name = "brave-search"
  protocol = "sse"
  url = "http://localhost:3001/sse"
  ```

- [ ] **examples/multi-server.toml**
  ```toml
  [[mcp.server]]
  name = "filesystem"
  protocol = "stdio"
  command = "npx"
  args = ["-y", "@modelcontextprotocol/server-filesystem", "./allowed-files"]

  [[mcp.server]]
  name = "brave-search"
  protocol = "sse"
  url = "http://localhost:3001/sse"

  [[mcp.server]]
  name = "postgres"
  protocol = "streamable"
  url = "http://localhost:3002/stream"
  [mcp.server.envs]
  DATABASE_URL = "postgresql://user:pass@localhost/db"
  ```

### 2.2 Provider-Specific Examples

- [ ] **examples/deepseek-cohere.toml** - Deepseek + Cohere embeddings
- [ ] **examples/openai-only.toml** - OpenAI with text-embedding-3-small
- [ ] **examples/multi-provider.toml** - All providers enabled

## Phase 3: Generation Scripts (Week 2)

### 3.1 Project Generation

- [ ] **scripts/generate-project.sh**
  ```bash
  #!/bin/bash
  # Main project generation script

  PROJECT_NAME="${1:-my-mcp-integration}"
  CONFIG_FILE="${2:-config.yaml}"

  # Generate with ggen
  ggen project gen \
      --template mcp-rig-integration \
      --output "$PROJECT_NAME" \
      --vars-file "$CONFIG_FILE"

  # Post-generation setup
  cd "$PROJECT_NAME"
  mkdir -p logs allowed-files
  cargo build
  ```

### 3.2 Server Management

- [ ] **scripts/add-mcp-server.sh**
  ```bash
  #!/bin/bash
  # Add new MCP server to existing project

  SERVER_NAME="$1"
  PROTOCOL="$2"

  case "$PROTOCOL" in
      stdio) add_stdio_server ;;
      sse) add_sse_server ;;
      streamable) add_streamable_server ;;
  esac
  ```

- [ ] **scripts/remove-mcp-server.sh**
  - Remove server from config.toml
  - Update dependencies if needed

### 3.3 Testing & Validation

- [ ] **scripts/test-integration.sh**
  ```bash
  #!/bin/bash
  # Integration testing script

  # Test config loading
  cargo test test_config_loading

  # Test MCP startup
  cargo test test_mcp_manager_startup

  # Test tool discovery
  cargo test test_tool_discovery

  # Test embeddings
  cargo test test_tool_embedding
  ```

- [ ] **scripts/validate-config.sh**
  - TOML syntax validation
  - MCP server reachability
  - API key validation

## Phase 4: Documentation (Week 3)

### 4.1 Core Documentation

- [x] **docs/ARCHITECTURE_SPECIFICATION.md** (completed)
  - System overview
  - Component architecture
  - Template specifications
  - Best practices

- [ ] **docs/TEMPLATES.md**
  - Template variable reference
  - Customization guide
  - Example configurations
  - Troubleshooting

- [ ] **docs/MCP_SERVERS.md**
  - Supported MCP servers
  - Transport configuration
  - Server-specific setup
  - Custom server development

- [ ] **docs/CUSTOMIZATION.md**
  - Adding LLM providers
  - Custom tool adaptors
  - Extending chat interface
  - Advanced configurations

### 4.2 User Guides

- [ ] **README.md** - Main project README
  - Quick start guide
  - Installation instructions
  - Usage examples
  - API reference

- [ ] **QUICK_START.md**
  - 5-minute setup
  - Basic example
  - Common patterns

- [ ] **ADVANCED_USAGE.md**
  - Multi-agent coordination
  - Custom embeddings
  - Tool result caching
  - Performance tuning

### 4.3 Developer Documentation

- [ ] **CONTRIBUTING.md**
  - Template contribution guide
  - Code standards
  - Testing requirements
  - PR process

- [ ] **API.md**
  - Module documentation
  - Type definitions
  - Error handling
  - Extension points

## Phase 5: Testing & Examples (Week 3)

### 5.1 Unit Tests

- [ ] Config loading tests
  ```rust
  #[tokio::test]
  async fn test_config_loading() {
      let config = Config::retrieve("test-config.toml").await.unwrap();
      assert!(!config.mcp.server.is_empty());
  }
  ```

- [ ] MCP manager tests
  ```rust
  #[tokio::test]
  async fn test_mcp_manager_startup() {
      let manager = create_test_manager().await;
      assert!(!manager.clients.is_empty());
  }
  ```

- [ ] Tool adaptor tests
  ```rust
  #[tokio::test]
  async fn test_tool_adaptation() {
      let tool = create_test_tool();
      let adaptor = McpToolAdaptor::new(tool, server);
      assert!(adaptor.definition().name == "test_tool");
  }
  ```

### 5.2 Integration Tests

- [ ] End-to-end chat flow
- [ ] Multi-server coordination
- [ ] Tool discovery and execution
- [ ] Embedding generation
- [ ] Error handling scenarios

### 5.3 Example Projects

- [ ] **examples/simple-chat/** - Basic chatbot
- [ ] **examples/filesystem-tools/** - File operations
- [ ] **examples/api-integration/** - External API calls
- [ ] **examples/multi-server/** - Multiple MCP servers
- [ ] **examples/custom-provider/** - Custom LLM provider

## Phase 6: Advanced Features (Week 4)

### 6.1 Enhanced Capabilities

- [ ] **Tool Result Caching**
  ```rust
  pub struct CachedToolAdaptor {
      inner: McpToolAdaptor,
      cache: Arc<RwLock<HashMap<String, String>>>,
  }
  ```

- [ ] **Multi-Agent Coordination**
  ```rust
  pub struct AgentCoordinator {
      agents: HashMap<String, Agent>,
      task_queue: TaskQueue,
  }
  ```

- [ ] **Custom Embedding Models**
  - Support for local models
  - Custom embedding endpoints
  - Fallback strategies

### 6.2 Monitoring & Observability

- [ ] Metrics collection
  ```rust
  pub struct MetricsCollector {
      tool_calls: Counter,
      response_time: Histogram,
      error_rate: Gauge,
  }
  ```

- [ ] Distributed tracing
  ```rust
  #[tracing::instrument]
  async fn call_tool(&self, args: String) -> String {
      // Traced execution
  }
  ```

- [ ] Performance dashboards
  - Grafana templates
  - Prometheus exporters

### 6.3 Developer Experience

- [ ] Interactive setup wizard
  ```bash
  ./scripts/setup-wizard.sh
  # Interactive prompts for configuration
  ```

- [ ] Hot-reload configuration
  ```rust
  pub struct HotReloadConfig {
      watcher: FileWatcher,
      reload_tx: Sender<Config>,
  }
  ```

- [ ] Visual tool builder
  - Web UI for tool configuration
  - Drag-and-drop server setup

## Phase 7: Quality Assurance (Week 4)

### 7.1 Code Quality

- [ ] Clippy linting (all warnings as errors)
- [ ] Rustfmt formatting
- [ ] Documentation coverage (>80%)
- [ ] Test coverage (>70%)

### 7.2 Performance Testing

- [ ] Benchmark suite
  ```rust
  #[bench]
  fn bench_tool_call(b: &mut Bencher) {
      b.iter(|| {
          // Benchmark tool execution
      });
  }
  ```

- [ ] Load testing
  - Concurrent chat sessions
  - High-frequency tool calls
  - Memory usage profiling

### 7.3 Security Audit

- [ ] Input validation
- [ ] API key handling
- [ ] Secure transport verification
- [ ] Dependency vulnerability scan

## Phase 8: Release Preparation (Week 5)

### 8.1 Package Publishing

- [ ] Crate versioning
- [ ] Changelog generation
- [ ] Release notes
- [ ] Tag creation

### 8.2 Distribution

- [ ] Docker images
  ```dockerfile
  FROM rust:1.75 as builder
  WORKDIR /app
  COPY . .
  RUN cargo build --release

  FROM debian:bookworm-slim
  COPY --from=builder /app/target/release/mcp-rig-integration /usr/local/bin/
  CMD ["mcp-rig-integration"]
  ```

- [ ] Binary releases (GitHub)
- [ ] Homebrew formula
- [ ] Cargo install documentation

### 8.3 Community

- [ ] Example repository
- [ ] Discord/Slack channel
- [ ] Issue templates
- [ ] Discussion forum

## Success Criteria

### Functional Requirements

- ✅ Generate complete MCP + Rig integration from templates
- ✅ Support stdio, SSE, and streamable transports
- ✅ Enable multiple LLM providers (Deepseek, Cohere, OpenAI)
- ✅ Implement RAG-based dynamic tool selection
- ✅ Provide streaming chat interface with tool visualization

### Quality Requirements

- ✅ Test coverage >70%
- ✅ Documentation coverage >80%
- ✅ Zero clippy warnings
- ✅ All templates validated
- ✅ Example projects functional

### Performance Requirements

- ✅ Tool discovery <1s
- ✅ Embedding generation <2s
- ✅ Chat response latency <500ms
- ✅ Multi-server startup <3s

## Timeline Summary

| Phase | Duration | Deliverables |
|-------|----------|--------------|
| 1. Templates | Week 1 | Core module templates, variables schema |
| 2. Examples | Week 2 | Pre-built configurations, provider examples |
| 3. Scripts | Week 2 | Generation, management, testing scripts |
| 4. Documentation | Week 3 | Architecture, usage, API docs |
| 5. Testing | Week 3 | Unit, integration, example tests |
| 6. Advanced | Week 4 | Caching, monitoring, DX improvements |
| 7. QA | Week 4 | Code quality, performance, security |
| 8. Release | Week 5 | Publishing, distribution, community |

**Total Duration:** 5 weeks

## Next Steps

1. **Immediate Actions**
   - [ ] Set up project structure
   - [ ] Create base templates
   - [ ] Implement core modules

2. **Week 1 Goals**
   - [ ] Complete all module templates
   - [ ] Validate template variable schema
   - [ ] Test basic generation

3. **Week 2 Goals**
   - [ ] Build example configurations
   - [ ] Create generation scripts
   - [ ] Document usage patterns

## Risk Mitigation

### Technical Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| MCP SDK API changes | High | Pin versions, provide migration guide |
| Rig framework updates | Medium | Abstract provider interface |
| Transport failures | High | Implement retry logic, fallbacks |
| Embedding API limits | Medium | Rate limiting, caching |

### Project Risks

| Risk | Impact | Mitigation |
|------|--------|------------|
| Scope creep | Medium | Strict phase boundaries |
| Template complexity | High | Modular design, clear docs |
| Testing coverage | High | TDD approach, CI/CD |
| Documentation lag | Medium | Doc-first development |

## Conclusion

This roadmap provides a structured approach to implementing a production-ready MCP + Rig integration example using ggen templates. The phased approach ensures quality at each stage while maintaining flexibility for iteration and improvement.

The success of this implementation will provide:
1. **For Users**: Easy-to-use templates for MCP + Rig projects
2. **For Developers**: Clear patterns for extension and customization
3. **For Community**: Reference implementation and best practices
