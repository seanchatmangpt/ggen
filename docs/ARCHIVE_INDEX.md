<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Documentation Archive Index](#documentation-archive-index)
  - [Directory Structure](#directory-structure)
    - [`/archive/thesis/` - Thesis & Research Documents](#archivethesis---thesis--research-documents)
    - [`/archive/agent-reports/` - Multi-Agent Execution Reports](#archiveagent-reports---multi-agent-execution-reports)
    - [`/archive/validation-reports/` - Validation & Benchmark Reports](#archivevalidation-reports---validation--benchmark-reports)
    - [`/archive/research/` - Research & Design Documents](#archiveresearch---research--design-documents)
    - [`/archive/infrastructure/` - Infrastructure & Deployment](#archiveinfrastructure---infrastructure--deployment)
    - [Root Archive Files](#root-archive-files)
  - [Migration to v6](#migration-to-v6)
  - [Why Archive These?](#why-archive-these)
  - [How to Use This Archive](#how-to-use-this-archive)
    - [Finding Specific Information](#finding-specific-information)
    - [For Active Development](#for-active-development)
    - [Maintenance Notes](#maintenance-notes)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Documentation Archive Index

This directory contains archived documentation, research, and historical reports that are preserved for reference but not part of the active project.

## Directory Structure

### `/archive/thesis/` - Thesis & Research Documents
- PhD thesis on ggen and Holographic Factory metaphor
- Dissertation guides and thesis expansion documents
- Technical accuracy reports and evidence manifestos

**Key Files:**
- `PhD_THESIS_TPS_GGEN.md` - Complete PhD thesis on TPS principles applied to ggen
- `THESIS-EXPANSION-COMPLETE.md` - Thesis expansion summary
- `DISSERTATION_GUIDE.md` - Dissertation development guide

### `/archive/agent-reports/` - Multi-Agent Execution Reports
- EPIC 9 execution reports from 10-agent parallel runs
- Individual agent analysis and summaries
- Collision detection and convergence reports

**Key Files:**
- `EPIC_9_FINAL_EXECUTION_REPORT.md` - Complete EPIC 9 execution results
- `AGENT-5-SUMMARY.md` - Agent 5 detailed analysis
- `agent-*.md` - Individual agent research and implementation

### `/archive/validation-reports/` - Validation & Benchmark Reports
- Adversarial validation comprehensive analysis
- Benchmarking standards and complete results
- Quality metrics and performance audits

**Key Files:**
- `ADVERSARIAL_VALIDATION_FINAL_REPORT.md` - Comprehensive validation
- `BENCHMARK_AUDIT.md` - Benchmark verification and auditing
- `80_20_SUMMARY.md` - Big Bang 80/20 implementation summary

### `/archive/research/` - Research & Design Documents
- V5 design strategy and architecture documents
- Research upgrade reports and conceptual frameworks
- Research index and evidence manifests

**Key Files:**
- `RESEARCH_INDEX.md` - Master research index
- `GGEN-V5-DESIGN.md` - V5 design documentation
- `RESEARCH-UPGRADE-KGC-LENS.md` - KGC-based upgrade analysis

### `/archive/infrastructure/` - Infrastructure & Deployment
- Docker deployment guides and GVisor integration
- FMEA failure mode analysis
- Debian package distribution and deployment guides
- Infrastructure generation guides for PaaS

**Key Files:**
- `FMEA_ggen_sync_user_simulation.md` - User experience FMEA analysis
- `DEB_GVISOR_REPORT.md` - GVisor containerization report
- `DOCKER_DEPLOY_NOW.md` - Docker deployment guide
- `GGEN_PAAS_GENERATION_GUIDE.md` - PaaS infrastructure generation

### Root Archive Files
- `IMPLEMENTATION_*.md` - Implementation reports and status
- `ITERATION-REPORT.md` - Development iteration summary
- `EVIDENCE_MANIFEST.md` - Proof of correctness manifesto
- `PR_*.md` - Pull request and merge readiness reports
- `GITHUB_*.md` - GitHub workflow documentation
- And additional supporting documents

## Migration to v6

The files in `/migration/` (not part of archive) are the **active** migration guides:
- `/migration/upgrade-guide.md` - Complete v5.1.0 → v6.0.0 upgrade path
- `/migration/breaking-changes.md` - Breaking changes reference
- `/migration/deliverables.md` - Migration deliverables summary

## Why Archive These?

These documents are preserved because they represent:
1. **Research & Development**: The intellectual journey of ggen's evolution
2. **Validation Evidence**: Proof of quality gates and performance claims
3. **Historical Context**: Understanding design decisions and trade-offs
4. **Academic Foundation**: PhD thesis and formal verification work
5. **Reference Material**: Future developers may need to understand past choices

## How to Use This Archive

### Finding Specific Information
1. Consult the appropriate subdirectory based on topic
2. Each subdirectory has a README-like description above
3. Use the Index (this file) as a navigation map

### For Active Development
- **Active docs**: See `/docs/` root and subdirectories like `/migration/`, `/reference/`
- **Core project constitution**: See `/CLAUDE.md` (root)
- **User guides**: See `README.md` (root) and `docs/INDEX.md`

### Maintenance Notes
All archive files are preserved non-destructively. They are organized but not deleted, making it possible to:
- Reference historical decisions
- Track evolution of ideas
- Validate past claims with original evidence
- Learn from previous development cycles

---

**Last Updated**: 2026-01-09  
**Archive Curator**: Claude Code v6  
**Purpose**: Preserve knowledge while maintaining clean project root
