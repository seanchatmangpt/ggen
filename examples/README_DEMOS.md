# Ggen + Cleanroom Demos 🚀

## Ultra-Fast Deployment Demo

**Goal:** Demonstrate <60 second concept-to-deploy workflow

### Quick Start

```bash
cd /Users/sac/ggen/examples

# Interactive demo (recommended for first time)
./ultra-deploy-demo.sh

# Or run specific scenario
./ultra-deploy-demo.sh --auto 1  # CLI Tool (30s)
./ultra-deploy-demo.sh --auto 2  # Library (35s)
./ultra-deploy-demo.sh --auto 3  # Web Service (55s)
```

### Files

| File | Purpose |
|------|---------|
| **ultra-deploy-demo.sh** | Main interactive demo with 3 scenarios |
| **screencast-demo.sh** | Recording-optimized version (slower pacing) |
| **test-demo.sh** | Automated testing of all scenarios |
| **ULTRA_DEPLOY_DEMO.md** | Comprehensive documentation |
| **DEMO_GUIDE.md** | Quick start guide and best practices |
| **README_DEMOS.md** | This file |

### Demo Scenarios

#### 1. CLI Tool (~30s)
Fast demonstration of command-line tool creation:
- Template selection
- Code generation
- Build & test
- Publish-ready output

#### 2. Library Crate (~35s)
Complete library development workflow:
- Full library structure
- Comprehensive docs
- Example code
- Test coverage

#### 3. Web Service (~55s)
Production-ready REST API:
- Axum web framework
- PostgreSQL database
- Docker containers
- Kubernetes deployment

### Features

- ⏱️ **Real-time timing** - See actual performance
- 🎨 **Colorful output** - ANSI colors and emojis
- 📊 **Performance metrics** - Detailed statistics
- 🚀 **Multiple scenarios** - Choose your use case
- 🎬 **Recording-ready** - Perfect for screencasts

### Use Cases

1. **Sales Demos** - Show speed and capability
2. **Conference Talks** - Impressive live demonstrations
3. **Onboarding** - Teach the workflow
4. **CI/CD** - Automated verification
5. **Documentation** - Video tutorials

### Integration

#### GitHub Actions
```yaml
- name: Demo
  run: ./examples/ultra-deploy-demo.sh --auto 1
```

#### Record to File
```bash
# Save output
./ultra-deploy-demo.sh --auto 1 | tee demo.log

# Record with script command
script -c "./ultra-deploy-demo.sh" demo-session.log
```

### Performance Targets

| Scenario | Target | Features |
|----------|--------|----------|
| CLI Tool | 30s | Fast, simple |
| Library | 35s | Comprehensive |
| Web Service | 55s | Production-ready |

**All scenarios complete in <60 seconds!**

### Documentation

- **Quick Start:** See this file
- **Detailed Guide:** Read `DEMO_GUIDE.md`
- **Technical Docs:** Check `ULTRA_DEPLOY_DEMO.md`

### Testing

```bash
# Verify all demos work
./test-demo.sh

# Expected output:
# ✓ Script exists and is executable
# Testing scenario 1...
#   ✓ Completed in 30s
# Testing scenario 2...
#   ✓ Completed in 35s
# Testing scenario 3...
#   ✓ Completed in 55s
# ✅ All tests passed!
```

### Customization

1. **Adjust timing:** Edit `sleep` values in scripts
2. **Add scenarios:** Create new `demo_*` functions
3. **Modify visuals:** Change colors and symbols
4. **Create variants:** Copy and customize scripts

### Success Metrics

✅ Completes in <60s
✅ Clear visual output
✅ Accurate metrics
✅ Works automated
✅ Recording-ready

### Best Practices

**For Demos:**
1. Practice beforehand
2. Check terminal size (80x24 minimum)
3. Test recording quality
4. Have manual backup
5. Explain as you go

**For Development:**
1. Keep demos updated
2. Test regularly
3. Match real performance
4. Add new scenarios
5. Document changes

## Other Examples

This directory also contains:
- `advanced-lifecycle-demo/` - Detailed lifecycle examples
- `full_demo.rs` - Rust code examples
- Additional demonstration materials

## Related Documentation

- **Ggen Docs:** `/Users/sac/ggen/docs/`
- **Cleanroom:** `/Users/sac/ggen/cleanroom/`
- **Templates:** `/Users/sac/ggen/docs/marketplace.md`

## Support

**Issues?** Check `DEMO_GUIDE.md` troubleshooting section

**Questions?** Read `ULTRA_DEPLOY_DEMO.md` for detailed info

**Contributions?** Submit PRs with new demo scenarios

---

**Run the demo now:**
```bash
./ultra-deploy-demo.sh
```

**Experience <60s concept-to-deploy!** 🚀
