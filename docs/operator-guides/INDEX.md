<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Operator Guides - Complete Index](#tps-operator-guides---complete-index)
  - [Quick Navigation by Role](#quick-navigation-by-role)
    - [For New Operators (First Week)](#for-new-operators-first-week)
    - [For On-Call Engineers (During Incidents)](#for-on-call-engineers-during-incidents)
    - [For Operations Managers (Daily/Weekly/Monthly)](#for-operations-managers-dailyweeklymonthly)
    - [For DevOps/SRE Engineers (Optimization & Tuning)](#for-devopssre-engineers-optimization--tuning)
    - [For Video Training (Visual Learners)](#for-video-training-visual-learners)
  - [Document Overview](#document-overview)
    - [01-quick-start-guide.md](#01-quick-start-guidemd)
    - [02-hands-on-labs.md](#02-hands-on-labsmd)
    - [03-troubleshooting-decision-trees.md](#03-troubleshooting-decision-treesmd)
    - [04-video-tutorial-scripts.md](#04-video-tutorial-scriptsmd)
    - [05-grafana-dashboard-guide.md](#05-grafana-dashboard-guidemd)
    - [06-operator-checklists.md](#06-operator-checklistsmd)
    - [07-configuration-tuning-guide.md](#07-configuration-tuning-guidemd)
  - [Quick Reference Tables](#quick-reference-tables)
    - [Symptom → Document Mapping](#symptom-%E2%86%92-document-mapping)
    - [Role → Document Mapping](#role-%E2%86%92-document-mapping)
  - [Roadmap: Using These Guides](#roadmap-using-these-guides)
    - [Week 1: Onboarding New Operator](#week-1-onboarding-new-operator)
    - [Month 1-3: Operational Excellence](#month-1-3-operational-excellence)
    - [Month 6: Advanced Operations](#month-6-advanced-operations)
  - [Document Maintenance](#document-maintenance)
    - [Review Schedule](#review-schedule)
    - [Contributing Updates](#contributing-updates)
  - [Integration with Other Systems](#integration-with-other-systems)
    - [Handbook Integration](#handbook-integration)
    - [Runbook Integration](#runbook-integration)
    - [Training Integration](#training-integration)
  - [Success Metrics](#success-metrics)
  - [Feedback & Continuous Improvement](#feedback--continuous-improvement)
  - [Quick Links](#quick-links)
  - [Version History](#version-history)
  - [Support & Escalation](#support--escalation)
  - [Printable Resources](#printable-resources)
  - [Next Steps](#next-steps)
  - [Contact & Support](#contact--support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Operator Guides - Complete Index

**Version**: 1.0
**Total Documents**: 8 (7 guides + 1 index)
**Total Pages**: ~200 pages equivalent
**Last Updated**: January 2026

---

## Quick Navigation by Role

### For New Operators (First Week)
Start here to get up to speed:

1. **[01-quick-start-guide.md](01-quick-start-guide.md)** (5 minutes)
   - Deploy TPS system to GKE in 5 minutes
   - Verify everything is working
   - Gain confidence before running production system

2. **[05-grafana-dashboard-guide.md](05-grafana-dashboard-guide.md)** (15 minutes)
   - Learn what each dashboard shows
   - Understand key metrics
   - Know what to look for when checking system health

3. **[02-hands-on-labs.md](02-hands-on-labs.md)** (3 hours total, 30 min each lab)
   - Lab 1: Jidoka (Circuit Breaking)
   - Lab 2: Kanban (Queue Management)
   - Lab 3: Andon (Problem Visibility)
   - Lab 4: Kaizen (Continuous Improvement)
   - Lab 5: Heijunka (Load Balancing)
   - Lab 6: Tracing (Distributed Tracing)

**Time Investment**: ~4 hours
**Outcome**: Deep understanding of all 6 TPS principles in action

---

### For On-Call Engineers (During Incidents)

Quick reference when something breaks:

1. **[03-troubleshooting-decision-trees.md](03-troubleshooting-decision-trees.md)** (Use during incidents)
   - Decision Tree 1: Queue Growing
   - Decision Tree 2: Circuit Breaker Open
   - Decision Tree 3: Error Rate High
   - Decision Tree 4: No Processing
   - Decision Tree 5: Latency Spike
   - Decision Tree 6: Scaling Issues
   - Emergency Procedures (complete system failure)

**When to Use**: Incident happens, use this to diagnose and fix
**Time**: 2-10 minutes per incident (varies by complexity)

---

### For Operations Managers (Daily/Weekly/Monthly)

Structured operational procedures:

1. **[06-operator-checklists.md](06-operator-checklists.md)** (Use daily/weekly/monthly)
   - Daily Operations (5-10 min)
   - Weekly Operations (30 min)
   - Monthly Audit (1-2 hours)
   - Ad-hoc checklists (incident response, deployments)

**When to Use**: Beginning of each shift, Monday morning, first of month
**Printable**: Yes - laminate for durability

---

### For DevOps/SRE Engineers (Optimization & Tuning)

Advanced configuration and optimization:

1. **[07-configuration-tuning-guide.md](07-configuration-tuning-guide.md)** (Reference while tuning)
   - Worker pool sizing
   - Queue configuration
   - Circuit breaker tuning
   - Timeout optimization
   - Rate limiting
   - Memory and resource configuration
   - Quick tuning checklist
   - Performance case studies

**When to Use**: After incident, during capacity planning, cost optimization
**Methodology**: Measure before → Change one parameter → Measure after

---

### For Video Training (Visual Learners)

Scripts for recording training videos:

1. **[04-video-tutorial-scripts.md](04-video-tutorial-scripts.md)** (5 videos, 8 min each = 40 min total)
   - Video 1: Deploy TPS System to Production
   - Video 2: Read and Interpret Dashboards
   - Video 3: Incident Response - Queue Growing
   - Video 4: Incident Response - Circuit Breaker Open
   - Video 5: Performance Tuning - Find and Fix Bottlenecks

**When to Use**: Onboarding new team members, training refresher
**Format**: Can be used as scripts for video recording

---

## Document Overview

### 01-quick-start-guide.md
**Purpose**: Get system running in 5 minutes
**Length**: ~2,500 words
**Sections**:
- Prerequisites check
- 5-minute deployment (5 steps)
- Verify observability
- Common issues
- Success summary

**Key Audiences**: New operators, onboarding engineers

---

### 02-hands-on-labs.md
**Purpose**: Learn TPS principles through hands-on experimentation
**Length**: ~5,000 words
**Sections**:
- Lab 1: Jidoka (autonomation)
- Lab 2: Kanban (queue management)
- Lab 3: Andon (visibility)
- Lab 4: Kaizen (continuous improvement)
- Lab 5: Heijunka (load balancing)
- Lab 6: Tracing (observability)

**Key Audiences**: Operators wanting deep understanding, new team members

---

### 03-troubleshooting-decision-trees.md
**Purpose**: Rapid diagnosis of production issues
**Length**: ~4,500 words
**Sections**:
- Decision Tree 1: Queue Growing
- Decision Tree 2: Circuit Breaker Open
- Decision Tree 3: Error Rate High
- Decision Tree 4: No Processing
- Decision Tree 5: Latency Spike
- Decision Tree 6: Scaling Issues
- Emergency procedures
- Summary table

**Key Audiences**: On-call engineers, incident responders

---

### 04-video-tutorial-scripts.md
**Purpose**: Scripts for training videos
**Length**: ~4,000 words
**Sections**:
- Video 1 (8 min): Deployment walkthrough
- Video 2 (8 min): Dashboard interpretation
- Video 3 (8 min): Queue incident response
- Video 4 (8 min): Circuit breaker incident response
- Video 5 (8 min): Performance tuning investigation

**Key Audiences**: Training coordinators, new operators (visual learners)

---

### 05-grafana-dashboard-guide.md
**Purpose**: Understand each dashboard and metric
**Length**: ~4,000 words
**Sections**:
- Dashboard 1: TPS Overview (8 panels)
- Dashboard 2: Jidoka (circuit breaker)
- Dashboard 3: Kanban (queue)
- Dashboard 4: Andon (alerts)
- Dashboard 5: Kaizen (performance)
- Dashboard 6: Heijunka (load balancing)
- Dashboard 7: Tracing (metrics)
- Alert thresholds
- Common dashboard combinations
- Troubleshooting dashboard issues

**Key Audiences**: All operators, system users

---

### 06-operator-checklists.md
**Purpose**: Structured daily/weekly/monthly operations
**Length**: ~3,500 words
**Sections**:
- Daily Operations (5-10 min checklist)
- Weekly Operations (30 min checklist)
- Monthly Operations (1-2 hour audit)
- Ad-hoc checklists (incident response, deployments)

**Key Audiences**: Operations managers, on-call engineers

---

### 07-configuration-tuning-guide.md
**Purpose**: Adjust system parameters for your workload
**Length**: ~4,500 words
**Sections**:
- Tuning philosophy
- Worker pool sizing
- Queue configuration
- Circuit breaker tuning
- Timeouts
- Rate limiting
- Metric collection
- Load balancing
- Memory and resources
- Quick tuning checklist
- Case studies
- Golden rules

**Key Audiences**: DevOps engineers, SREs, system architects

---

## Quick Reference Tables

### Symptom → Document Mapping

| Symptom | Document | Section |
|---------|----------|---------|
| Queue growing | Decision Trees | Tree 1 |
| Circuit breaker open | Decision Trees | Tree 2 |
| Error rate high | Decision Trees | Tree 3 |
| No processing | Decision Trees | Tree 4 |
| Latency spike | Decision Trees | Tree 5 |
| Scaling issues | Decision Trees | Tree 6 |
| Don't know how to read dashboard | Dashboard Guide | Overview |
| Worker pool not right | Config Tuning | Section 1 |
| Queue filling too fast | Config Tuning | Section 2 |
| Want to optimize cost | Config Tuning | Checklist |

---

### Role → Document Mapping

| Role | Primary Documents | Time Investment |
|------|-------------------|-----------------|
| New Operator | Quick Start + Dashboard Guide + Labs | 4-6 hours |
| On-Call Engineer | Decision Trees + Checklists | 30 min (once learned) |
| Operations Manager | Checklists (daily/weekly/monthly) | 45 min/day, 2 hours/week, 4 hours/month |
| DevOps/SRE | Config Tuning + Decision Trees | 1-2 hours per tuning session |
| Training Coordinator | Video Scripts | 5-10 hours (one-time to record) |

---

## Roadmap: Using These Guides

### Week 1: Onboarding New Operator

**Monday** (2 hours):
- Read: Quick Start Guide
- Do: Deploy system to GKE
- Result: System running, confidence built

**Tuesday-Wednesday** (4 hours):
- Complete: All 6 hands-on labs (30 min each)
- Result: Deep understanding of TPS principles

**Thursday** (1 hour):
- Read: Dashboard Guide
- Do: Monitor system for 1 hour, identify each metric
- Result: Know how to read dashboards

**Friday** (1 hour):
- Review: Decision Trees
- Mock incident: Practice using decision trees
- Result: Ready for on-call rotation

**Total**: 8 hours
**Ready for**: Operational tasks, basic troubleshooting

---

### Month 1-3: Operational Excellence

**Daily** (10 min):
- Morning: Daily checklist from Operator Checklists

**Weekly** (30 min):
- Monday: Weekly checklist from Operator Checklists

**As Needed**:
- Incident: Use Decision Trees to diagnose and fix
- Tuning: Reference Configuration Tuning Guide

**Result**: Confident operations, good incident response

---

### Month 6: Advanced Operations

**Monthly** (2 hours):
- Audit: Full monthly checklist

**Quarterly** (4 hours):
- Optimization: Use Config Tuning Guide to optimize system
- Case studies: Study provided performance case studies

**Result**: Cost optimization, performance tuning expertise

---

## Document Maintenance

### Review Schedule

**Quarterly Review**:
- Check if procedures still accurate
- Add new failure modes discovered
- Update metric thresholds if needed
- Update case studies with new real-world examples

**Annual Audit**:
- Complete rewrite of all documents
- Incorporate new features and changes
- Update all commands and examples
- Ensure all decision trees still accurate

### Contributing Updates

**Found an Issue?**
1. Note the document and section
2. File issue with:
   - What's wrong (outdated command, wrong metric, unclear instruction)
   - Where (document name, line number or section)
   - What should be (suggested fix)
3. Submit PR with updated content

---

## Integration with Other Systems

### Handbook Integration
These guides complement the main TPS Operations Handbook:
- **Handbook**: Architecture, principles, deep dives
- **Guides**: Step-by-step procedures, quick reference

### Runbook Integration
These guides reference specific runbooks:
- **Decision Trees**: Link to runbook-incident-response.md
- **Checklists**: Link to runbook-deployment.md, runbook-capacity-planning.md
- **Dashboard Guide**: Link to alert rules and their runbooks

### Training Integration
- **Certification Program**: Uses labs from hands-on-labs.md
- **Video Training**: Uses scripts from video-tutorial-scripts.md
- **Onboarding**: Uses quick-start-guide.md

---

## Success Metrics

After implementing these guides, you should achieve:

**Onboarding**:
- ✓ New operators productive in 1 week (vs 4 weeks previously)
- ✓ Onboarding cost reduced 75%
- ✓ First incident response time < 5 minutes

**Operations**:
- ✓ MTTR (Mean Time To Repair) < 10 minutes
- ✓ False alert rate < 5%
- ✓ Incident repeatability addressed (same issue doesn't recur)

**Knowledge**:
- ✓ All operators know how to read dashboards
- ✓ All operators can run daily checklist
- ✓ 80% of operators can troubleshoot independently

---

## Feedback & Continuous Improvement

**What's Working?**
- Document what operators find most valuable
- Expand those sections

**What's Missing?**
- New failure modes discovered?
- New operational procedures needed?
- Add new documents or sections

**Examples of Additions**:
- New decision tree for "Database Latency"
- New lab for "Chaos Testing"
- New checklist for "Monthly Security Audit"

---

## Quick Links

| Resource | Location | Purpose |
|----------|----------|---------|
| Handbook | handbook-tps-operations.md | Principles, architecture, deep dives |
| Certification | certification-tps-operator.md | Training program, exams |
| Runbooks | runbook-*.md | Step-by-step incident procedures |
| Glossary | tps-glossary.md | Term definitions |
| FAQ | tps-faq.md | Common questions |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | Jan 25, 2026 | Initial release: 7 guides + index |
| 0.9 | Jan 20, 2026 | Draft for review |
| 0.8 | Jan 15, 2026 | Outline and structure |

---

## Support & Escalation

**For Questions About**:
- **Guides**: Open issue in repository or reach out to Training Team
- **System Behavior**: Check Decision Trees and Troubleshooting Guide
- **Configuration**: See Configuration Tuning Guide
- **Incidents**: Use Decision Trees + on-call runbooks
- **Training**: Contact Training Coordinator for Video Tutorials

---

## Printable Resources

**Recommended to Print & Laminate**:
- [Operator Checklists](06-operator-checklists.md) (daily, weekly, monthly)
- [Decision Trees Summary Table](03-troubleshooting-decision-trees.md#summary-table)
- [Dashboard Interpretation Cheat Sheet](05-grafana-dashboard-guide.md#summary-table)

**Size**: Print at 8.5x11" or 11x17" (fits in one pocket)
**Material**: Laminate for durability (operators reference during incidents)
**Update**: Reprint quarterly or when major procedures change

---

## Next Steps

1. **New Operators**: Start with Quick Start Guide
2. **On-Call Engineers**: Study Decision Trees and Checklists
3. **Operations Managers**: Set up daily/weekly/monthly checklist schedules
4. **DevOps/SREs**: Bookmark Configuration Tuning Guide for reference
5. **Training Coordinators**: Schedule video recording using provided scripts

---

**Status**: Production-Ready
**Tested On**: GKE 1.25+ with n1-standard-4 nodes
**Audience**: SRE/DevOps engineers (Rust/Erlang familiarity not required)
**Total Training Time**: ~8 hours (new operator to productive)

---

## Contact & Support

- **Documentation Issues**: File PR or issue in repository
- **Operational Questions**: Ask in #tps-operations Slack channel
- **Training Requests**: Contact training@example.com
- **Incident Support**: On-call engineer handles (use Decision Trees)

---

**Last Updated**: January 25, 2026
**Maintainer**: TPS Operations Training Team
**License**: Internal Use Only
