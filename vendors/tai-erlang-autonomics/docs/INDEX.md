# TAI Autonomics Documentation Index

**Phase**: 1 (Evaluation/Free Tier)
**Status**: Production Ready
**Last Updated**: 2026-01-26

---

## Quick Navigation

### For Customers Starting Out

1. **[SUPPORT_MODEL.md](./SUPPORT_MODEL.md)** - Start here
   - What kind of support do we offer?
   - What should you expect?
   - How do you get help?
   - **Read time**: 2-3 minutes

2. **[INSTALL.md](./INSTALL.md)** - Deployment options
   - Three ways to deploy (Cloud Run, Docker, Local)
   - 5-minute quick start
   - Troubleshooting guide
   - **Read time**: 5-10 minutes

3. **[API.md](./API.md)** - Integration guide
   - HTTP endpoints reference
   - MCP tools reference
   - Example curl commands
   - Request/response schemas
   - **Read time**: 8-15 minutes

4. **[ROADMAP.md](./ROADMAP.md)** - Future direction
   - Phase 1 vs Phase 2 vs Phase 3 vs Phase 4
   - Pricing and tiers
   - Feature matrix
   - **Read time**: 5-10 minutes

---

## Customer Journey Map

### Day 1: Discovery
**Question**: "What is TAI Autonomics and how does support work?"
**Document**: [SUPPORT_MODEL.md](./SUPPORT_MODEL.md)
**Time**: 2-3 minutes
**Outcome**: Understand MCP-only support model, no human support

### Day 2: Installation
**Question**: "How do I deploy this?"
**Document**: [INSTALL.md](./INSTALL.md)
**Time**: 5-10 minutes
**Outcome**: Service running on Cloud Run or local Docker

### Day 3: Integration
**Question**: "How do I integrate with my system?"
**Document**: [API.md](./API.md)
**Time**: 8-15 minutes
**Outcome**: POST to /marketplace endpoint or use MCP tools

### Day 4+: Planning
**Question**: "What's next? How do we scale? When can we upgrade?"
**Document**: [ROADMAP.md](./ROADMAP.md)
**Time**: 5-10 minutes
**Outcome**: Understand Phase 1 → Phase 2 upgrade path, pricing

---

## Key Facts At-a-Glance

### Support Model
- **Type**: MCP-only (no human support in Phase 1)
- **Response Time**: Deterministic (same issue = same fix)
- **Receipts**: Hash-chained, audit-ready
- **Cost**: Free (Phase 1 evaluation)

### Deployment
- **Cloud Run**: 5 minutes
- **Docker**: 5-10 minutes
- **Local Erlang**: 15-30 minutes
- **Prerequisites**: gcloud CLI (required), Docker/Erlang (optional)

### API
- **HTTP Endpoints**: 3 (/health, /marketplace, /pubsub)
- **MCP Tools**: 4 (health.check, entitlement.apply_event, receipts.verify_chain, support.model)
- **Rate Limit**: 100 req/s per tenant
- **Authentication**: Optional API key (Phase 2)

### Roadmap
- **Phase 1** (Now): MCP-only, free, evaluation
- **Phase 2** (Q2 2026): Insurance-backed, $499-$9,999+/month
- **Phase 3** (Q3 2026): 75+ recipes, Kaizen
- **Phase 4** (Q4 2026): Capability packs, $1M ARR target

---

## Common Questions & Which Doc to Read

| Question | Document | 
|----------|----------|
| What kind of support do you offer? | SUPPORT_MODEL |
| Can I talk to a human? | SUPPORT_MODEL |
| How long does deployment take? | INSTALL |
| How do I integrate? | API |
| When do you have human support? | ROADMAP |
| What's the pricing? | ROADMAP |
| Can I run this on-premises? | INSTALL |
| How do I troubleshoot? | INSTALL |

---

## Contact

**Sales & Licensing**: sales@taiautonomics.io
**Technical (Phase 1)**: MCP tools only (taiea.health.check, taiea.entitlement.apply_event, taiea.receipts.verify_chain)

---

*Documentation for TAI Autonomics Phase 1 Launch - Created 2026-01-26*
