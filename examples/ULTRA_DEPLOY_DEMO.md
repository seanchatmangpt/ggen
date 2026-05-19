# Ultra-Fast Deployment Demo 🚀

**Demonstrates:** <60 second concept-to-deploy workflow using ggen + cleanroom synergy

## Quick Start

```bash
# Make executable
chmod +x ultra-deploy-demo.sh

# Run interactive demo
./ultra-deploy-demo.sh

# Run specific scenario non-interactively
./ultra-deploy-demo.sh --auto 1  # CLI Tool
./ultra-deploy-demo.sh --auto 2  # Library
./ultra-deploy-demo.sh --auto 3  # Web Service
```

## Demo Scenarios

### 1. CLI Tool (~30s)
**Target Time:** 30 seconds
**Demonstrates:** Simple command-line tool with argument parsing

**Workflow:**
1. **Concept (3s)** - Select rust-cli-basic template
2. **Generate (5s)** - Create project structure with ggen
3. **Test (12s)** - Build, test, and validate in cleanroom
4. **Validate (5s)** - Run clippy, fmt, and package checks
5. **Deploy (5s)** - Simulate cargo publish

**Output:**
- Cargo.toml with metadata
- src/main.rs with clap integration
- Comprehensive tests
- README and documentation
- Ready-to-publish package

### 2. Library Crate (~35s)
**Target Time:** 35 seconds
**Demonstrates:** Reusable library with comprehensive documentation

**Workflow:**
1. **Concept (4s)** - Select rust-lib-full template
2. **Generate (6s)** - Create library structure with examples
3. **Test (15s)** - Build, test (100% coverage), generate docs, run examples
4. **Validate (5s)** - Clippy, package validation
5. **Deploy (5s)** - Simulate cargo publish

**Output:**
- Library crate with public API
- Comprehensive documentation
- Multiple examples
- Full test coverage
- CI/CD configuration
- Ready-to-publish package

### 3. Web Service (~55s)
**Target Time:** 55 seconds
**Demonstrates:** Production-ready REST API with database

**Workflow:**
1. **Concept (5s)** - Select rust-axum-api template
2. **Generate (10s)** - Create full-stack web service
3. **Test (25s)** - Build, Docker setup, integration tests, health checks
4. **Validate (8s)** - Clippy, Docker build, Dockerfile linting
5. **Deploy (7s)** - Simulate container registry push and k8s deploy

**Output:**
- Axum web service with routes
- PostgreSQL integration
- Docker and docker-compose
- Kubernetes manifests
- Integration test suite
- Production-ready deployment

## Performance Metrics

Each scenario displays:
- ⏱️ **Total Time** - Actual execution time
- 🎯 **Target Time** - Expected completion time
- ⚡ **Time Saved** - How much faster than target
- 📊 **Efficiency** - Percentage improvement
- ✓ **Stage Breakdown** - Time per stage

## Features Demonstrated

### Core Capabilities
- ✅ **Template-Based Generation** - Instant project scaffolding
- ✅ **Integrated Testing** - Cleanroom validation
- ✅ **Automatic Validation** - Quality checks built-in
- ✅ **Production Ready** - Deploy-ready output
- ✅ **Full Pipeline** - Concept to deployment

### Visual Features
- 🎨 **Colorful Output** - ANSI colors for clarity
- ⏱️ **Real-Time Timing** - Stage and total timers
- 🔄 **Progress Indicators** - Clear status updates
- ✨ **Success Animations** - Celebration on completion
- 📊 **Metrics Display** - Performance statistics

## Technical Details

### Implementation
- **Language:** Bash shell script
- **Dependencies:** Basic Unix utilities (date, sleep)
- **Terminal:** ANSI color support recommended
- **Platform:** macOS, Linux, WSL2

### Demo Flow
```
┌─────────┐
│ Concept │ (3-5s)  - Select template
└────┬────┘
     │
┌────▼────┐
│Generate │ (5-10s) - Create project with ggen
└────┬────┘
     │
┌────▼────┐
│  Test   │ (12-25s) - Build & validate in cleanroom
└────┬────┘
     │
┌────▼────┐
│Validate │ (5-8s)  - Quality checks
└────┬────┘
     │
┌────▼────┐
│ Deploy  │ (5-7s)  - Publish/deploy
└─────────┘
```

## Usage Patterns

### Interactive Mode (Default)
```bash
./ultra-deploy-demo.sh

# Presents menu:
# 1) CLI Tool
# 2) Library Crate
# 3) Web Service
# 4) Run All Scenarios
# q) Quit
```

### Automated Mode
```bash
# Run specific scenario
./ultra-deploy-demo.sh --auto 1

# Use in CI/CD
./ultra-deploy-demo.sh --auto 2 | tee demo-output.log

# Batch testing
for i in 1 2 3; do
  ./ultra-deploy-demo.sh --auto $i
done
```

## Customization

### Adjust Timings
Edit sleep values in each stage to match actual operations or speed up demo:

```bash
# Current: Realistic timing
sleep 2

# For faster demo
sleep 0.5

# For actual operations
$(actual_command) # Remove sleep, use real commands
```

### Add New Scenarios
Add a new demo function following the pattern:

```bash
demo_my_scenario() {
    print_scenario "My Scenario" "Description" "45"

    show_stage "1/5" "${BRAIN} Selecting template..."
    # ... implementation
    show_success

    # ... remaining stages
}
```

### Modify Display
Customize colors and symbols at the top of the script:

```bash
# Change colors
GREEN='\033[0;32m'
RED='\033[0;31m'

# Change symbols
CHECK="✓"
ROCKET="🚀"
```

## Integration with CI/CD

### GitHub Actions
```yaml
- name: Run Ultra-Deploy Demo
  run: |
    chmod +x examples/ultra-deploy-demo.sh
    ./examples/ultra-deploy-demo.sh --auto 1
```

### GitLab CI
```yaml
demo:
  script:
    - chmod +x examples/ultra-deploy-demo.sh
    - ./examples/ultra-deploy-demo.sh --auto 2
```

### Recording Output
```bash
# With script command
script -c "./ultra-deploy-demo.sh --auto 3" demo.log

# With tee
./ultra-deploy-demo.sh --auto 1 | tee demo-output.txt
```

## Performance Goals

| Scenario | Target | Typical | Best Case |
|----------|--------|---------|-----------|
| CLI Tool | 30s | 28-32s | 25s |
| Library | 35s | 33-37s | 30s |
| Web Service | 55s | 52-58s | 48s |

**Overall Goal:** <60 seconds for any scenario

## Success Criteria

✅ **Demo completes under 60s**
✅ **All stages execute successfully**
✅ **Visual output is clear and engaging**
✅ **Metrics display accurately**
✅ **Can run unattended**

## Troubleshooting

### Colors Not Showing
```bash
# Check terminal support
echo $TERM

# Force color output
export TERM=xterm-256color
```

### Script Not Executable
```bash
chmod +x ultra-deploy-demo.sh
```

### Timing Issues
Adjust sleep values if demo runs too fast/slow for your terminal emulator.

## Related Documentation

- **Ggen CLI:** `./docs/cli.md`
- **Cleanroom:** `./cleanroom/README.md`
- **Templates:** `./docs/marketplace.md`
- **Workflow:** `./docs/development-workflow.md`

## Demo Script

This demonstration is designed to be:
- **Impressive** - Shows speed and capability
- **Educational** - Explains each step
- **Realistic** - Mirrors actual workflow
- **Executable** - Can be run anytime
- **Measurable** - Provides clear metrics

**Perfect for:** Sales demos, conference talks, onboarding, showcasing capabilities

---

**Experience the future of Rust development: <60s concept to deployed!** 🚀
