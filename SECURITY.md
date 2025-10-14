# Security Policy

## Reporting a Security Issue

The ggen project takes security seriously and welcomes security reports. We are committed to providing prompt attention to security issues.

**⚠️ IMPORTANT**: Security issues should be reported privately and NOT via public GitHub Issues.

### How to Report

Please report security vulnerabilities by email to:
- **Email**: security@ggen.dev (or open a private security advisory on GitHub)
- **Response Time**: We aim to acknowledge reports within 48 hours
- **Updates**: We'll keep you informed about the fix timeline

### What to Include

A good security report should include:
- Description of the vulnerability
- Steps to reproduce the issue
- Potential impact assessment
- Any suggested fixes (optional)
- Your contact information for follow-up

## Security Considerations for ggen Users

### Template Execution Risk ⚠️

**ggen executes templates with full system access**. Only use templates from trusted sources:

```bash
# ⚠️ DANGER: This executes arbitrary code
ggen gen untrusted-template.tmpl

# ✅ SAFE: Review template content first
cat suspicious-template.tmpl  # Review before execution
ggen gen suspicious-template.tmpl
```

### AI-Generated Code Safety

**AI-generated templates may contain vulnerabilities**:
- Always review AI-generated code before production use
- Test AI-generated templates in isolated environments
- Validate all inputs and outputs
- Use ggen's cleanroom testing framework for validation

### Command Injection Vectors

**Template variables can be exploited** if not properly sanitized:

```yaml
# ⚠️ DANGER: User input in shell commands
---
to: "run.sh"
---
#!/bin/bash
{{user_command}}  # This could be "; rm -rf /"
```

**Best Practice**: Sanitize all user inputs in templates.

## Vulnerability Coordination

Remediation of security vulnerabilities is prioritized by the project team. The project team coordinates remediation with third-party stakeholders via [GitHub Security Advisories](https://docs.github.com/en/code-security/security-advisories/guidance-on-reporting-and-writing/about-coordinated-disclosure-of-security-vulnerabilities).

Third-party stakeholders may include:
- The reporter of the issue
- Affected direct or indirect users of ggen
- Maintainers of upstream dependencies (if applicable)
- Marketplace template authors (for template vulnerabilities)

## Security Advisories

The project team is committed to transparency in the security issue disclosure process. We announce security issues via:

- [GitHub Security Advisories](https://github.com/seanchatmangpt/ggen/security/advisories)
- [GitHub Release Notes](https://github.com/seanchatmangpt/ggen/releases)
- Project documentation updates

## Security Best Practices

### For Template Authors

1. **Input Validation**: Always validate and sanitize template variables
2. **Least Privilege**: Request minimal permissions required
3. **Dependency Audits**: Regularly audit template dependencies
4. **Testing**: Test templates in cleanroom environments
5. **Documentation**: Document security implications clearly

### For Template Users

1. **Source Trust**: Only use templates from trusted sources
2. **Code Review**: Review template content before execution
3. **Isolation**: Test templates in isolated environments first
4. **Updates**: Keep ggen and templates up to date
5. **Monitoring**: Monitor generated code for unexpected behavior

### For ggen Developers

1. **Error Handling**: Use Result-based error propagation
2. **Input Sanitization**: Sanitize all external inputs
3. **Secure Defaults**: Default to secure configurations
4. **Dependency Management**: Keep dependencies updated
5. **Code Review**: All code changes require security review

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.x     | :white_check_mark: |
| < 1.0   | :x:                |

## Known Security Considerations

### Post-Quantum Cryptography

ggen uses ML-DSA (Dilithium3) for post-quantum signatures. While this provides quantum resistance, the implementation is subject to ongoing cryptographic research.

### Testcontainers Security

The cleanroom framework uses Docker containers via testcontainers. Ensure Docker daemon is properly secured:
- Use Docker socket with appropriate permissions
- Run containers with minimal capabilities
- Isolate container networks
- Regularly update Docker and container images

### RDF and SPARQL Injection

**SPARQL queries constructed from user input can be exploited**:

```rust
// ⚠️ DANGER: SPARQL injection vulnerability
let query = format!("SELECT ?x WHERE {{ ?x foaf:name '{}' }}", user_input);

// ✅ SAFE: Use parameterized queries
let query = sparql_builder()
    .select("?x")
    .where_clause("?x foaf:name ?name")
    .bind("name", user_input)
    .build();
```

## Security Checklist for Production

Before deploying ggen in production:

- [ ] Review all templates for security issues
- [ ] Validate AI-generated code in isolated environments
- [ ] Configure appropriate resource limits
- [ ] Enable security policies in cleanroom tests
- [ ] Audit all dependencies for known vulnerabilities
- [ ] Set up monitoring and alerting
- [ ] Document security procedures for your team
- [ ] Test disaster recovery procedures
- [ ] Review and update security policies regularly

## Contact

For security-related questions that don't involve vulnerabilities:
- GitHub Discussions: https://github.com/seanchatmangpt/ggen/discussions
- Documentation: https://seanchatmangpt.github.io/ggen/

---

**Last Updated**: 2025-10-13
**Version**: 1.0
