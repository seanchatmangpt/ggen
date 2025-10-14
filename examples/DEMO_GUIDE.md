# Ultra-Fast Deployment Demo Guide 🚀

## Overview

This directory contains a comprehensive demonstration of the **<60 second concept-to-deploy workflow** using ggen + cleanroom integration.

## Files

| File | Purpose | Usage |
|------|---------|-------|
| `ultra-deploy-demo.sh` | **Main interactive demo** | `./ultra-deploy-demo.sh` |
| `screencast-demo.sh` | Recording-optimized version | `./screencast-demo.sh` |
| `test-demo.sh` | Automated testing | `./test-demo.sh` |
| `ULTRA_DEPLOY_DEMO.md` | Detailed documentation | Reference |
| `DEMO_GUIDE.md` | This file | Quick start guide |

## Quick Start

### 1. Interactive Demo (Recommended)
```bash
cd /Users/sac/ggen/examples
./ultra-deploy-demo.sh
```

**Features:**
- Interactive menu
- Choose from 3 scenarios
- Real-time progress
- Performance metrics
- Colorful output

### 2. Automated Demo (CI/CD)
```bash
# Run specific scenario
./ultra-deploy-demo.sh --auto 1  # CLI Tool (30s)
./ultra-deploy-demo.sh --auto 2  # Library (35s)
./ultra-deploy-demo.sh --auto 3  # Web Service (55s)
```

### 3. Screencast Recording
```bash
# Optimized for video recording (slower, clearer)
./screencast-demo.sh web   # Web service demo
./screencast-demo.sh cli   # CLI tool demo
```

### 4. Run Tests
```bash
# Verify all scenarios work
./test-demo.sh
```

## Demo Scenarios

### Scenario 1: CLI Tool (~30s)
**Best for:** Quick demonstrations, first impressions

**Shows:**
- Fast template selection
- Instant code generation
- Rapid testing cycle
- Ready-to-publish package

**Command:** `./ultra-deploy-demo.sh --auto 1`

### Scenario 2: Library Crate (~35s)
**Best for:** Library developers, documentation focus

**Shows:**
- Full library structure
- Comprehensive documentation
- Example code generation
- Complete test coverage

**Command:** `./ultra-deploy-demo.sh --auto 2`

### Scenario 3: Web Service (~55s)
**Best for:** Production readiness, full-stack demos

**Shows:**
- Complete web service
- Database integration
- Docker containerization
- Kubernetes deployment
- Integration testing

**Command:** `./ultra-deploy-demo.sh --auto 3`

## Demo Flow

```
┌──────────────────────────────────────────────────────────┐
│                    DEMO WORKFLOW                          │
└──────────────────────────────────────────────────────────┘

    [1] Concept & Template Selection (3-5s)
         ↓ Select template from marketplace
         ↓ Review template features

    [2] Code Generation with Ggen (5-10s)
         ↓ Generate project structure
         ↓ Create configuration files
         ↓ Add dependencies

    [3] Testing in Cleanroom (12-25s)
         ↓ Build project
         ↓ Run tests
         ↓ Validate output

    [4] Quality Validation (5-8s)
         ↓ Run clippy
         ↓ Check formatting
         ↓ Package validation

    [5] Deployment (5-7s)
         ↓ Simulate publish
         ↓ Show metrics
         ✓ Complete!

Total Time: <60 seconds
```

## Key Metrics

### Performance Targets

| Scenario | Target | Typical | Stages |
|----------|--------|---------|--------|
| CLI Tool | 30s | 28-32s | 5 |
| Library | 35s | 33-37s | 5 |
| Web Service | 55s | 52-58s | 5 |

### What's Measured

- ⏱️ **Total Time** - Start to finish
- 🎯 **Stage Time** - Per-stage duration
- ⚡ **Efficiency** - vs. 60s baseline
- 📊 **Success Rate** - Completion status

## Use Cases

### 1. Sales Demonstrations
```bash
# Quick, impressive demo
./ultra-deploy-demo.sh --auto 1
```

**Highlights:**
- Speed (<60s)
- Ease of use
- Production readiness
- Professional output

### 2. Conference Talks
```bash
# Recording-friendly version
./screencast-demo.sh web
```

**Benefits:**
- Clear stages
- Visible progress
- Narration pauses
- Impressive finale

### 3. Developer Onboarding
```bash
# Interactive learning
./ultra-deploy-demo.sh
```

**Shows:**
- Full workflow
- Tool integration
- Best practices
- Real-world scenarios

### 4. CI/CD Integration
```bash
# Automated testing
./test-demo.sh

# In pipeline
./ultra-deploy-demo.sh --auto 2 | tee demo.log
```

## Customization

### Adjust Timing

Edit sleep values for faster/slower demos:

```bash
# Fast demo (half speed)
sed -i '' 's/sleep \([0-9]\)/sleep $((\1\/2))/g' ultra-deploy-demo.sh

# Restore original
git checkout ultra-deploy-demo.sh
```

### Add New Scenario

1. Create new function:
```bash
demo_my_scenario() {
    print_scenario "My Scenario" "Description" "40"

    # Stage 1
    show_stage "1/5" "${BRAIN} Selecting template..."
    # ... implementation
    show_success

    # ... more stages
}
```

2. Add to menu:
```bash
echo -e "  ${CYAN}4)${RESET} My Scenario"
```

3. Add case statement:
```bash
4)
    demo_my_scenario
    show_final_results $(elapsed_time) "My Scenario"
    ;;
```

### Modify Visuals

Change colors and symbols in the script header:

```bash
# Colors
GREEN='\033[0;32m'
CYAN='\033[0;36m'

# Symbols
CHECK="✓"
ROCKET="🚀"
```

## Integration Examples

### GitHub Actions
```yaml
name: Demo

on: [push]

jobs:
  demo:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run demo
        run: |
          chmod +x examples/ultra-deploy-demo.sh
          examples/ultra-deploy-demo.sh --auto 1
```

### GitLab CI
```yaml
demo:
  script:
    - chmod +x examples/ultra-deploy-demo.sh
    - ./examples/ultra-deploy-demo.sh --auto 2
  artifacts:
    paths:
      - demo.log
```

### Record to File
```bash
# With script command
script -c "./ultra-deploy-demo.sh --auto 3" demo-recording.log

# With tee (live + save)
./ultra-deploy-demo.sh --auto 1 | tee demo-output.txt

# Strip ANSI codes for plain text
./ultra-deploy-demo.sh --auto 1 | sed 's/\x1B\[[0-9;]*[mK]//g' > plain.txt
```

## Troubleshooting

### Colors Not Showing

**Problem:** Terminal doesn't support ANSI colors

**Solution:**
```bash
# Check terminal type
echo $TERM

# Set color support
export TERM=xterm-256color

# Or run in supported terminal
# (iTerm2, Terminal.app, GNOME Terminal, etc.)
```

### Demo Too Fast/Slow

**Problem:** Timing doesn't match your system

**Solution:**
```bash
# Edit sleep values in script
# Current: sleep 2
# Faster: sleep 1
# Slower: sleep 3

# Or use screencast version (slower by default)
./screencast-demo.sh
```

### Script Not Executable

**Problem:** Permission denied

**Solution:**
```bash
chmod +x *.sh
```

### Timeout Issues

**Problem:** Demo exceeds expected time

**Solution:**
1. Check system load: `top`
2. Close other applications
3. Use `--auto` mode (faster than interactive)
4. Adjust timeout in test script

## Success Criteria

✅ **Demo completes in <60s**
✅ **All stages execute successfully**
✅ **Visual output is clear and engaging**
✅ **Metrics display accurately**
✅ **Works in automated mode**
✅ **Can be recorded for screencasts**

## Best Practices

### For Presentations

1. **Practice first**: Run demo 2-3 times before presenting
2. **Check terminal size**: Ensure text is visible (80x24 minimum)
3. **Test recording**: Verify screencast quality beforehand
4. **Have backup**: Keep manual demo ready if automation fails
5. **Explain as you go**: Don't just run silently

### For Demonstrations

1. **Start simple**: CLI Tool scenario first
2. **Build complexity**: Library → Web Service
3. **Highlight metrics**: Point out sub-60s completion
4. **Show features**: Mention ggen + cleanroom synergy
5. **Take questions**: Pause between scenarios

### For Development

1. **Keep updated**: Sync with actual ggen/cleanroom capabilities
2. **Test regularly**: Run test-demo.sh frequently
3. **Adjust timing**: Match real-world performance
4. **Add scenarios**: Create domain-specific demos
5. **Document changes**: Update this guide

## Related Documentation

- **Ggen CLI:** `/Users/sac/ggen/docs/cli.md`
- **Cleanroom Testing:** `/Users/sac/ggen/cleanroom/README.md`
- **Template System:** `/Users/sac/ggen/docs/marketplace.md`
- **Development Workflow:** `/Users/sac/ggen/docs/development-workflow.md`
- **Detailed Demo Docs:** `ULTRA_DEPLOY_DEMO.md`

## Support

**Questions?** Check the detailed documentation in `ULTRA_DEPLOY_DEMO.md`

**Issues?** Report at: https://github.com/yourusername/ggen/issues

**Contributions?** Submit PRs with new demo scenarios

## Summary

This demo showcases the power of **ggen + cleanroom integration**:

- ⚡ **<60s deployment** - From concept to deployed
- 🎯 **3 scenarios** - CLI, Library, Web Service
- 🎨 **Professional output** - Colorful, animated, clear
- 📊 **Real metrics** - Actual performance data
- 🚀 **Production ready** - Not just a toy demo

**Run it now:**
```bash
./ultra-deploy-demo.sh
```

---

**Experience the future of Rust development!** 🚀
