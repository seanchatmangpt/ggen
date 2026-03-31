# Security Policy

## Reporting Security Vulnerabilities

We take security seriously. If you discover a security vulnerability in TAI Autonomics, please report it responsibly to us.

### How to Report

**Do not create public GitHub issues for security vulnerabilities.** Instead:

1. **Email**: security@tai-autonomics.dev
2. **Security Contact**: See [.well-known/security.txt](/.well-known/security.txt)
3. **CVSS Score**: Include CVSS v3.1 score if available
4. **Timeline**: We commit to responding within 48 hours

### What to Include

When reporting a vulnerability, please provide:

- Description of the vulnerability
- Steps to reproduce (if applicable)
- Affected components and versions
- CVSS score (if known)
- Proof of concept code (optional)
- Suggested remediation (if you have one)

### Responsible Disclosure

We follow responsible disclosure practices:

1. **Initial Response**: Within 48 hours
2. **Acknowledgment**: Within 5 business days
3. **Patch Development**: 14-30 days depending on severity
4. **Coordinated Disclosure**: We'll work with you on timing
5. **Public Announcement**: After patch is released

## Supported Versions

| Version | Status | Security Updates Until |
|---------|--------|----------------------|
| 1.0.0   | Current | 2026-12-31 |
| 0.9.0   | LTS | 2026-06-30 |
| 0.8.0   | EOL | Ended 2024-12-31 |

### Security Update Timeline

- **Critical (CVSS 9.0-10.0)**: Patch within 24 hours
- **High (CVSS 7.0-8.9)**: Patch within 7 days
- **Medium (CVSS 4.0-6.9)**: Patch within 30 days
- **Low (CVSS 0.1-3.9)**: Patch in next release

## Security Best Practices

### For Developers

1. **Dependencies**: Keep dependencies updated
   ```bash
   rebar3 deps --update
   cargo-make check  # Verify no warnings
   ```

2. **Code Review**: All code requires security review
   - No hardcoded secrets
   - No unsafe patterns
   - Proper error handling

3. **Testing**: Comprehensive security testing required
   - Unit tests with >80% coverage
   - Integration tests
   - Security-focused tests

4. **Commits**: Never commit secrets
   - Use `.env.example` for configuration
   - Use GitHub Secrets for CI/CD
   - Rotate access tokens regularly

### For Operators

1. **Access Control**
   - Use service accounts with minimal permissions
   - Rotate credentials regularly
   - Enable audit logging

2. **Network Security**
   - Use VPC for isolation
   - Enable Cloud Armor for DDoS protection
   - Use Cloud NAT for outbound traffic control

3. **Data Protection**
   - Enable encryption at rest (Firestore)
   - Enable encryption in transit (TLS 1.2+)
   - Minimize sensitive data retention

4. **Monitoring**
   - Enable Cloud Audit Logs
   - Monitor for suspicious activity
   - Set up security alerts

## Vulnerability Disclosure Timeline

When a vulnerability is reported:

### Day 0-1: Initial Assessment
- Confirm vulnerability exists
- Assess severity (CVSS score)
- Identify affected versions

### Day 2-3: Initial Response
- Send confirmation to reporter
- Provide estimated timeline for patch
- Discuss coordinated disclosure

### Day 4-30: Patch Development
- Develop and test security patch
- Perform code review
- Prepare security advisory

### Day 31-45: Coordinated Release
- Request 72-hour advance notice to major users
- Publish security advisory
- Release patch in new version
- Public disclosure

### Day 46+: Post-Disclosure
- Monitor for exploitation
- Provide guidance for users
- Plan longer-term improvements

## Security Scanning

We perform continuous security scanning:

### Automated Scans
- **Dependency Audit**: Daily (GitHub Dependabot)
- **OWASP ZAP**: On every push
- **Container Scan**: On every build (Trivy)
- **Code Quality**: On every commit (Elvis, Dialyzer)
- **Secret Detection**: Continuous (TruffleHog, git-secrets)
- **License Compliance**: Weekly

### Manual Reviews
- Security-focused code reviews on all PRs
- Annual penetration testing
- Quarterly security audit

## Cryptographic Practices

### Key Management
- All cryptographic operations use strong algorithms
- Keys are stored securely (never in code)
- Rotation schedule: Every 90 days

### Algorithms
- **Signing**: RS256 (RSA 2048-bit minimum)
- **Encryption**: AES-256-GCM
- **Hashing**: SHA-256
- **Random Generation**: Cryptographically secure

## Authentication & Authorization

### API Authentication
- JWT tokens for API authentication
- Token expiration: 1 hour
- Refresh token expiration: 30 days
- Rotation on compromise

### Authorization
- Role-based access control (RBAC)
- Principle of least privilege
- Audit logging for all access

## Infrastructure Security

### Cloud Infrastructure (GCP)
- VPC for network isolation
- Cloud Armor for DDoS protection
- Cloud NAT for outbound IP control
- Service accounts with minimal permissions
- Encryption at rest and in transit

### Secrets Management
- Cloud Secret Manager for all secrets
- Automatic rotation where supported
- Access logging

## Incident Response

If a vulnerability is actively exploited:

1. **Immediate Action** (0-2 hours)
   - Assess impact and scope
   - Notify affected users
   - Begin emergency patch development

2. **Short-term** (2-24 hours)
   - Release emergency patch
   - Provide mitigation guidance
   - Publish security advisory

3. **Medium-term** (1-7 days)
   - Post-incident analysis
   - Implement preventive measures
   - Update documentation

4. **Long-term** (1-30 days)
   - Update security policies
   - Implement additional scanning
   - Provide educational resources

## Compliance & Standards

We comply with:
- **OWASP Top 10**: Regular reviews and fixes
- **CWE Top 25**: Monitored and addressed
- **NIST Cybersecurity Framework**: Aligned implementation
- **Cloud Security Alliance**: Best practices followed

## Security Questions & Contact

For security-related questions (non-vulnerability):
- Email: security@tai-autonomics.dev
- Security Team: See [.well-known/security.txt](/.well-known/security.txt)

## Changelog

### Version 1.0.0 (Current)
- Initial security policy
- Continuous vulnerability scanning
- 24-hour critical response SLA

---

**Last Updated**: 2026-01-26
**Policy Version**: 1.0.0
