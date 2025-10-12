# Production Readiness Guide

Production deployment validation and requirements tracking for ggen projects.

## Overview

The production readiness system ensures safe deployments by tracking requirements and validating that critical features are implemented before deployment.

## 80/20 Rule Categories

### 🚨 Critical (20% effort, 80% value)
- **Authentication & Authorization** - User login/logout, JWT tokens
- **Error Handling** - Comprehensive error types, no unwrap/expect in production
- **Health Checks** - HTTP endpoints for load balancer monitoring
- **Input Validation** - Sanitization and security validation
- **Database Migrations** - Schema versioning and rollback capability

### ⚠️ Important (30% effort, 15% value)
- **API Documentation** - OpenAPI/Swagger specifications
- **Unit Testing** - Comprehensive test coverage (>80%)
- **Integration Testing** - Component interaction validation
- **Performance Monitoring** - Metrics collection and alerting
- **Configuration Management** - Environment-based configuration

### ℹ️ Nice-to-Have (50% effort, 5% value)
- **Rate Limiting** - API abuse prevention
- **Advanced Caching** - Redis-based caching strategies
- **Circuit Breakers** - External service failure handling
- **Advanced Security** - CSRF, CORS, security headers

## Commands

```bash
# Check current production readiness status
ggen lifecycle readiness

# Check only critical requirements
ggen lifecycle readiness --critical-only

# Update requirement status
ggen lifecycle readiness-update auth-basic complete

# Show placeholders that need implementation
ggen lifecycle placeholders --category critical

# Validate for deployment
ggen lifecycle validate --env production --strict
```

## Deployment Gates

### Development Environment
- **Minimum Score**: 40%
- **Validation**: Basic functionality checks
- **Purpose**: Fast iteration during development

### Staging Environment
- **Minimum Score**: 60%
- **Validation**: Integration testing, performance checks
- **Purpose**: Pre-production validation

### Production Environment
- **Minimum Score**: 75%
- **Validation**: All critical requirements, security scanning
- **Purpose**: Production deployment safety

## Integration

The production readiness system integrates with:
- **Lifecycle phases** - Validation hooks before deployment
- **Marketplace packages** - Template validation
- **CI/CD pipelines** - Automated readiness checks

## Example Workflow

```bash
# 1. Check current status (initially low)
ggen lifecycle readiness
# Output: Overall Score: 15.2%

# 2. Implement critical features
# - Add authentication
# - Add error handling
# - Add health checks

# 3. Update status as features complete
ggen lifecycle readiness-update auth-basic complete
ggen lifecycle readiness-update error-handling complete

# 4. Re-check status (should be much higher)
ggen lifecycle readiness
# Output: Overall Score: 78.5%

# 5. Deploy when >75% ready
ggen lifecycle run deploy --env production
```
