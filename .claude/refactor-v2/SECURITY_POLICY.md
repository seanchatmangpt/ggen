# Security Policy - ggen v1.2.0

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.2.x   | :white_check_mark: |
| 1.1.x   | :x:                |
| < 1.0   | :x:                |

## Reporting a Vulnerability

**DO NOT** report security vulnerabilities through public GitHub issues.

Instead, please report them via:
- Email: security@ggen.io (preferred)
- GitHub Security Advisories: https://github.com/seanchatmangpt/ggen/security/advisories

You should receive a response within 48 hours. If the issue is confirmed, we will:
1. Release a patch within 7 days for critical vulnerabilities
2. Release a patch within 30 days for moderate vulnerabilities
3. Publicly disclose after patch is available

## Security Best Practices

### For Users

1. **Keep ggen updated**: Always use the latest stable version
2. **Validate inputs**: When using ggen in scripts, validate all user inputs
3. **Least privilege**: Run ggen with minimal required permissions
4. **Review templates**: Audit third-party templates before use
5. **Secure storage**: Protect .ggen config files and credentials

### For Contributors

1. **Input validation**: All user inputs MUST be validated
2. **Error handling**: Never use .unwrap() in production code paths
3. **Path safety**: Use validate_file_path() for all path inputs
4. **Dependencies**: Run cargo audit before submitting PRs
5. **Tests**: Include security test cases for new features

## Known Security Issues

### Accepted Risks (Development Only)

#### tokio-tar (RUSTSEC-2025-0111)
- **Severity**: CRITICAL in general, MEDIUM for ggen
- **Scope**: Development/testing only (testcontainers)
- **Status**: ACCEPTED - Not used in production builds
- **Mitigation**: Monitoring upstream for fixes
- **Tracking**: Issue #XXX

### Mitigation Strategies

1. **tokio-tar**: Used only in dev-dependencies via testcontainers
2. **Unmaintained deps**: Monitoring for maintained alternatives
3. **Path traversal**: Comprehensive validation in place
4. **Input injection**: Strict character validation

## Security Features

### Input Validation
- Path traversal detection (Component::ParentDir)
- Path length limits (1000 chars max)
- Character whitelist validation
- Project name validation
- Format enum validation

### Error Handling
- No panics in production code (CI enforced)
- Graceful error propagation
- Security-focused error messages
- No sensitive data in logs

### Dependency Management
- Weekly automated vulnerability scans
- Cargo audit in CI pipeline
- Dependency review for PRs
- Version pinning for stability

## Security Checklist (for PRs)

Before submitting a PR that touches security-sensitive code:

- [ ] All user inputs are validated
- [ ] No .unwrap() or .expect() in production code
- [ ] All paths use validate_file_path()
- [ ] cargo audit passes (with accepted ignores)
- [ ] cargo clippy --deny unwrap_used passes
- [ ] Security tests included
- [ ] No secrets in code or tests
- [ ] Error messages don't leak sensitive info
- [ ] Documentation updated

## Vulnerability Disclosure Timeline

1. **Day 0**: Vulnerability reported
2. **Day 2**: Confirmation and severity assessment
3. **Day 7**: Patch developed and tested (critical)
4. **Day 14**: Patch released (critical)
5. **Day 30**: Public disclosure
6. **Day 30+**: Recognition and credit

## Security Hall of Fame

We acknowledge security researchers who help improve ggen:

- (Your name here - report responsibly!)

## Contact

- Security Email: security@ggen.io
- Encrypted: [PGP Key] (coming soon)
- Matrix: #ggen-security:matrix.org (coming soon)

---

Last Updated: 2025-11-01
