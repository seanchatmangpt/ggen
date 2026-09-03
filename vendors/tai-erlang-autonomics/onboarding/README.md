# Value-Based Pricing Onboarding Platform

30-day automated onboarding system that gets customers live on value-based pricing with confidence, transparency, and measurable outcomes.

## Overview

The onboarding platform guides customers through five critical stages:
1. **Setup Wizard** - Deploy pricing integration in 5 steps
2. **Value Definition** - Customers define what "value" means in their context
3. **Receipt Validation** - Prove measurement accuracy with cryptographic receipts
4. **Approval Workflow** - CFO/CTO reviews and signs off
5. **Go-Live** - Switch to value-based billing with safety gates

## Features

### Core Components
- **Setup Wizard** - 5-step deployment guide with validation at each stage
- **Value Definition UI** - Interactive interface for business metrics configuration
- **Receipt System** - Cryptographic validation of measurement accuracy
- **Approval Workflow** - Multi-stakeholder sign-off process
- **Go-Live Switch** - Safety gates and rollback capability
- **Real-Time Dashboard** - Live value visualization with alerts
- **Alert Management** - Proactive notifications on value thresholds
- **REST API** - Programmatic integration for complex systems
- **Support Ticket System** - Track questions, build FAQ automation
- **Feedback Loop** - Continuous measurement accuracy improvement

### Quality Standards
- **100% Type Safety** - Full TypeScript type coverage
- **80%+ Test Coverage** - Chicago TDD with real objects
- **Security First** - OAuth2, JWT, RBAC, audit logging
- **Observable** - OTEL instrumentation on all critical paths
- **Resilient** - Circuit breakers, retries, graceful degradation
- **Scalable** - Stateless services, horizontal scaling ready
- **Beautiful UX** - Intuitive, trustworthy, accessible

## Tech Stack

### Frontend
- **React 18** - Modern UI framework
- **TypeScript** - Full type safety
- **Tailwind CSS** - Responsive design
- **React Query** - Data fetching and caching
- **React Router** - Client-side navigation
- **Zustand** - State management
- **Zod** - Runtime validation

### Backend
- **Node.js/Express** - REST API server
- **PostgreSQL** - Persistent storage
- **Redis** - Caching and sessions
- **OpenTelemetry** - Observability
- **Passport** - Authentication
- **Joi** - Schema validation
- **Winston** - Structured logging

### DevOps
- **Docker** - Containerization
- **Docker Compose** - Local development
- **Kubernetes** - Production orchestration
- **Terraform** - Infrastructure as code
- **GitHub Actions** - CI/CD pipeline

## Quick Start

### Prerequisites
```bash
- Node.js 18+
- Docker & Docker Compose
- PostgreSQL 14+
- Redis 7+
```

### Development

```bash
# Start local stack
docker-compose up -d

# Install dependencies
npm install

# Run migrations
npm run db:migrate

# Start dev server
npm run dev

# Run tests
npm test

# Run linting
npm run lint

# Type check
npm run typecheck
```

### Build & Deploy

```bash
# Build production images
docker build -f Dockerfile -t onboarding:prod .

# Deploy to Kubernetes
kubectl apply -f k8s/

# Verify deployment
kubectl get pods -n onboarding
```

## API Documentation

### Authentication
All endpoints require JWT Bearer token or OAuth2 credentials.

```bash
POST /api/v1/auth/login
POST /api/v1/auth/logout
POST /api/v1/auth/refresh
```

### Setup Wizard
```bash
POST /api/v1/customers
GET /api/v1/customers/:id
POST /api/v1/customers/:id/setup/step1
POST /api/v1/customers/:id/setup/step2
POST /api/v1/customers/:id/setup/step3
POST /api/v1/customers/:id/setup/step4
POST /api/v1/customers/:id/setup/step5
```

### Value Definition
```bash
POST /api/v1/customers/:id/value-definitions
GET /api/v1/customers/:id/value-definitions
PUT /api/v1/customers/:id/value-definitions/:definitionId
DELETE /api/v1/customers/:id/value-definitions/:definitionId
```

### Receipts & Validation
```bash
GET /api/v1/customers/:id/receipts
GET /api/v1/customers/:id/receipts/:receiptId
POST /api/v1/customers/:id/receipts/validate
```

### Approvals
```bash
POST /api/v1/customers/:id/approvals
GET /api/v1/customers/:id/approvals
PUT /api/v1/customers/:id/approvals/:approvalId/approve
PUT /api/v1/customers/:id/approvals/:approvalId/reject
```

### Dashboard
```bash
GET /api/v1/customers/:id/dashboard/summary
GET /api/v1/customers/:id/dashboard/value-trend
GET /api/v1/customers/:id/dashboard/alerts
```

### Support
```bash
POST /api/v1/customers/:id/tickets
GET /api/v1/customers/:id/tickets
GET /api/v1/customers/:id/tickets/:ticketId
POST /api/v1/customers/:id/tickets/:ticketId/reply
```

## Project Structure

```
onboarding/
├── frontend/                 # React SPA
│   ├── public/
│   ├── src/
│   │   ├── components/      # Reusable UI components
│   │   ├── pages/           # Page components
│   │   ├── hooks/           # Custom React hooks
│   │   ├── services/        # API clients
│   │   ├── stores/          # Zustand stores
│   │   ├── types/           # TypeScript types
│   │   ├── utils/           # Utilities
│   │   └── App.tsx
│   ├── tests/               # Jest tests
│   └── package.json
├── backend/                 # Node.js/Express API
│   ├── src/
│   │   ├── controllers/     # Request handlers
│   │   ├── services/        # Business logic
│   │   ├── models/          # Data models
│   │   ├── middleware/      # Express middleware
│   │   ├── routes/          # Route definitions
│   │   ├── utils/           # Utilities
│   │   ├── types/           # TypeScript types
│   │   ├── config/          # Configuration
│   │   └── server.ts
│   ├── migrations/          # Database migrations
│   ├── tests/               # Jest tests
│   └── package.json
├── shared/                  # Shared types and utilities
│   ├── types.ts
│   ├── schemas.ts
│   └── constants.ts
├── docker-compose.yml       # Local development stack
├── Dockerfile              # Production image
├── k8s/                    # Kubernetes manifests
│   ├── deployment.yaml
│   ├── service.yaml
│   ├── ingress.yaml
│   ├── configmap.yaml
│   └── secrets.yaml
├── terraform/              # Infrastructure as code
│   ├── main.tf
│   ├── variables.tf
│   └── outputs.tf
└── docs/                   # Documentation
    ├── API.md
    ├── ARCHITECTURE.md
    ├── DEPLOYMENT.md
    └── SECURITY.md
```

## Development Guidelines

### Code Quality
- **TypeScript** - Strict mode required
- **Linting** - ESLint with Prettier
- **Testing** - Jest with 80%+ coverage
- **Type Safety** - No `any` types without justification
- **Error Handling** - Comprehensive try-catch blocks
- **Logging** - Structured JSON logging on critical paths

### Testing Strategy
- **Unit Tests** - Test individual functions and services
- **Integration Tests** - Test API endpoints and database interactions
- **E2E Tests** - Playwright for full user journeys
- **Contract Tests** - Validate API contracts
- **Performance Tests** - Load testing with k6

### Deployment
- **Semantic Versioning** - MAJOR.MINOR.PATCH
- **Docker Images** - Tagged with version and git sha
- **Database Migrations** - Versioned and reversible
- **Secrets Management** - Never commit secrets, use env vars
- **Health Checks** - Liveness and readiness probes
- **Rollback Plan** - Always have rollback capability

## Monitoring & Observability

### Metrics
- Request latency (p50, p95, p99)
- Error rates by endpoint
- Database query performance
- Cache hit rates
- Customer onboarding progress
- Value calculation accuracy

### Logging
- Structured JSON logs
- Request/response logging
- Error stack traces
- Audit trail for approvals
- Support ticket interactions

### Tracing
- Distributed traces across services
- Value calculation flow
- Approval workflow transitions
- API integration calls

## Security

### Authentication & Authorization
- OAuth2 for customer identity
- JWT for API access
- RBAC for customer roles (Admin, Finance, Technical)
- Session management with secure cookies

### Data Protection
- Encryption at rest (PostgreSQL)
- Encryption in transit (TLS)
- Secrets in encrypted env vars
- PII handling compliance
- GDPR data retention policies

### Audit & Compliance
- Immutable audit trail
- Cryptographic receipts
- Approval tracking
- API access logging
- Security scanning in CI/CD

## Support

### Documentation
- [API Reference](./docs/API.md)
- [Architecture Guide](./docs/ARCHITECTURE.md)
- [Deployment Guide](./docs/DEPLOYMENT.md)
- [Security Guide](./docs/SECURITY.md)
- [FAQ & Troubleshooting](./docs/FAQ.md)

### Community
- GitHub Issues for bug reports
- GitHub Discussions for feature requests
- Email support@onboarding.io for urgent issues

## License

Apache-2.0

---

**Built for trust, measured for value, designed for success.**
