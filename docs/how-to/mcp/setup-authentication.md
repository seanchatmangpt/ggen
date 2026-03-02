<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Setup Authentication](#how-to-setup-authentication)
  - [Authentication Methods Overview](#authentication-methods-overview)
  - [API Key Authentication](#api-key-authentication)
    - [Generate API Key](#generate-api-key)
    - [Configure API Key](#configure-api-key)
    - [Use API Key in Requests](#use-api-key-in-requests)
  - [JWT Authentication](#jwt-authentication)
    - [Generate JWT Secret](#generate-jwt-secret)
    - [Configure JWT](#configure-jwt)
    - [Generate JWT Token](#generate-jwt-token)
    - [Verify JWT Token](#verify-jwt-token)
  - [Mutual TLS Authentication](#mutual-tls-authentication)
    - [Generate Certificates](#generate-certificates)
    - [Configure mTLS](#configure-mtls)
    - [Test mTLS Connection](#test-mtls-connection)
  - [OAuth 2.0 Integration](#oauth-20-integration)
    - [Configure OAuth 2.0](#configure-oauth-20)
    - [Supported Providers](#supported-providers)
  - [Token Management](#token-management)
    - [List Active Tokens](#list-active-tokens)
    - [Revoke Tokens](#revoke-tokens)
    - [Refresh Tokens](#refresh-tokens)
  - [Security Best Practices](#security-best-practices)
  - [Testing Authentication](#testing-authentication)
    - [Test API Key Authentication](#test-api-key-authentication)
    - [Test JWT Authentication](#test-jwt-authentication)
  - [Troubleshooting](#troubleshooting)
    - [Authentication Failed](#authentication-failed)
    - [Token Expired](#token-expired)
    - [Certificate Errors](#certificate-errors)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Setup Authentication

Guide to configuring authentication for MCP server connections.

## Authentication Methods Overview

ggen MCP supports multiple authentication methods:

| Method | Use Case | Security Level |
|--------|----------|----------------|
| API Key | Simple service-to-service | Medium |
| JWT | Token-based with claims | High |
| mTLS | Mutual certificate verification | Very High |
| OAuth 2.0 | Third-party integration | High |

## API Key Authentication

**Problem:** You need simple authentication for service-to-service communication.

**Solution:** Configure API key authentication.

### Generate API Key

```bash
# Generate a new API key
ggen mcp auth generate-api-key

# Generate with description
ggen mcp auth generate-api-key --description "Production server"

# Generate with expiration
ggen mcp auth generate-api-key --expires-in 30d
```

**Expected Output:**
```bash
$ ggen mcp auth generate-api-key --description "Production server"

Generated API Key: ggen_sk_prod_abc123xyz789

Description: Production server
Expires: 2025-03-09

Store this key securely. It will not be shown again.
```

### Configure API Key

```bash
# Set API key in environment
export GGEN_MCP_API_KEY="ggen_sk_prod_abc123xyz789"

# Add to configuration file
ggen mcp config set auth.api_key="ggen_sk_prod_abc123xyz789"

# Add to server configuration
# In .mcp.json:
{
  "mcp_servers": {
    "production": {
      "env": {
        "GGEN_MCP_API_KEY": "ggen_sk_prod_abc123xyz789"
      }
    }
  }
}
```

### Use API Key in Requests

```bash
# With curl
curl -H "Authorization: Bearer ggen_sk_prod_abc123xyz789" \
  http://localhost:3000/tools

# With ggen CLI
ggen mcp list --api-key "ggen_sk_prod_abc123xyz789"
```

## JWT Authentication

**Problem:** You need token-based authentication with claims and expiration.

**Solution:** Configure JWT authentication.

### Generate JWT Secret

```bash
# Generate a new JWT secret
ggen mcp auth generate-jwt-secret

# Store secret securely
export GGEN_MCP_JWT_SECRET="$(ggen mcp auth generate-jwt-secret)"
```

### Configure JWT

```bash
# Add to configuration
ggen mcp config set \
  auth.jwt.secret="$GGEN_MCP_JWT_SECRET" \
  auth.jwt.issuer="ggen-mcp" \
  auth.jwt.audience="mcp-clients" \
  auth.jwt.expiration=3600
```

**Configuration file:**
```json
{
  "auth": {
    "jwt": {
      "secret": "your-secret-key",
      "issuer": "ggen-mcp",
      "audience": "mcp-clients",
      "expiration": 3600,
      "algorithm": "HS256"
    }
  }
}
```

### Generate JWT Token

```bash
# Generate token for client
ggen mcp auth generate-token \
  --client-id "my-client" \
  --permissions "read,write" \
  --expires-in 1h

# Generate token with custom claims
ggen mcp auth generate-token \
  --client-id "my-client" \
  --claims '{"admin": true, "tools": ["agent-start", "workflow-start"]}'
```

**Expected Output:**
```bash
$ ggen mcp auth generate-token --client-id "my-client" --permissions "read,write"

Generated JWT Token:
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJjbGllbnRfaWQiOiJteS1jbGllbnQiLCJwZXJtaXNzaW9ucyI6WyJyZWFkIiwid3JpdGUiXSwiZXhwIjoxNzA5NDkxMjAwfQ.abc123xyz789

Token expires in: 1 hour
```

### Verify JWT Token

```bash
# Verify token validity
ggen mcp auth verify-token "$TOKEN"

# Decode token (for debugging)
ggen mcp auth decode-token "$TOKEN"
```

## Mutual TLS Authentication

**Problem:** You need certificate-based authentication for high-security scenarios.

**Solution:** Configure mTLS authentication.

### Generate Certificates

```bash
# Generate CA certificate
ggen mcp auth generate-ca --out ca.crt

# Generate server certificate
ggen mcp auth generate-cert \
  --ca ca.crt \
  --cn "mcp-server" \
  --out server.crt \
  --key-out server.key

# Generate client certificate
ggen mcp auth generate-cert \
  --ca ca.crt \
  --cn "client-1" \
  --out client.crt \
  --key-out client.key
```

### Configure mTLS

```bash
# Add to configuration
ggen mcp config set \
  auth.mtls.enabled=true \
  auth.mtls.ca_cert="/path/to/ca.crt" \
  auth.mtls.server_cert="/path/to/server.crt" \
  auth.mtls.server_key="/path/to/server.key"
```

**Configuration file:**
```json
{
  "auth": {
    "mtls": {
      "enabled": true,
      "ca_cert": "/path/to/ca.crt",
      "server_cert": "/path/to/server.crt",
      "server_key": "/path/to/server.key",
      "verify_client": true,
      "verify_hostname": true
    }
  }
}
```

### Test mTLS Connection

```bash
# Test with client certificate
curl --cert client.crt \
  --key client.key \
  --cacert ca.crt \
  https://localhost:8443/tools

# Test with ggen CLI
ggen mcp list \
  --client-cert client.crt \
  --client-key client.key \
  --ca-cert ca.crt
```

## OAuth 2.0 Integration

**Problem:** You need to integrate with external OAuth 2.0 providers.

**Solution:** Configure OAuth 2.0 as a proxy authentication method.

### Configure OAuth 2.0

```json
{
  "auth": {
    "oauth2": {
      "enabled": true,
      "provider": "github",
      "client_id": "your-client-id",
      "client_secret": "your-client-secret",
      "redirect_uri": "http://localhost:3000/callback",
      "scopes": ["read:org", "user:email"]
    }
  }
}
```

### Supported Providers

- GitHub
- Google
- GitLab
- Microsoft
- Okta
- Custom OpenID Connect providers

```bash
# Configure GitHub OAuth
ggen mcp config set auth.oauth2.provider=github
ggen mcp config set auth.oauth2.client_id="your-github-client-id"

# Configure Google OAuth
ggen mcp config set auth.oauth2.provider=google
ggen mcp config set auth.oauth2.client_id="your-google-client-id"
```

## Token Management

### List Active Tokens

```bash
# List all API keys
ggen mcp auth list-api-keys

# List all JWT tokens
ggen mcp auth list-tokens

# List with details
ggen mcp auth list --verbose
```

### Revoke Tokens

```bash
# Revoke API key
ggen mcp auth revoke-api-key "ggen_sk_prod_abc123xyz789"

# Revoke JWT token
ggen mcp auth revoke-token "$TOKEN"

# Revoke all tokens for client
ggen mcp auth revoke-client "my-client"
```

### Refresh Tokens

```bash
# Refresh expired token
ggen mcp auth refresh-token "$TOKEN"

# Auto-refresh on expiration
ggen mcp config set auth.jwt.auto_refresh=true
```

## Security Best Practices

1. **Never hardcode secrets** in configuration files
   ```bash
   # Bad
   export GGEN_MCP_API_KEY="sk_prod_abc123"

   # Good
   export GGEN_MCP_API_KEY="$(cat /run/secrets/mcp-api-key)"
   ```

2. **Use environment variables** for sensitive data
   ```bash
   export GGEN_MCP_API_KEY
   export GGEN_MCP_JWT_SECRET
   ```

3. **Rotate credentials regularly**
   ```bash
   # Set rotation schedule
   ggen mcp auth set-rotation-policy --days 30 --warn-before 7
   ```

4. **Use strong secrets**
   ```bash
   # Generate with 256-bit entropy
   ggen mcp auth generate-jwt-secret --bits 256
   ```

5. **Limit token scope**
   ```bash
   # Grant minimal permissions
   ggen mcp auth generate-token \
     --permissions "tools:read,agent:start"
   ```

## Testing Authentication

### Test API Key Authentication

```bash
# Test with valid key
curl -H "Authorization: Bearer $API_KEY" \
  http://localhost:3000/tools

# Test with invalid key
curl -H "Authorization: Bearer invalid-key" \
  http://localhost:3000/tools
```

### Test JWT Authentication

```bash
# Generate token
TOKEN=$(ggen mcp auth generate-token --client-id "test")

# Test with token
curl -H "Authorization: Bearer $TOKEN" \
  http://localhost:3000/tools

# Test expired token
curl -H "Authorization: Bearer expired-token" \
  http://localhost:3000/tools
```

## Troubleshooting

### Authentication Failed

**Problem:** Requests return 401 Unauthorized.

**Solutions:**
```bash
# Verify credentials
ggen mcp auth verify-token "$TOKEN"

# Check API key is valid
ggen mcp auth list-api-keys | grep "your-key"

# Verify server configuration
ggen mcp config show

# Check logs for errors
ggen mcp server logs --tail 100
```

### Token Expired

**Problem:** JWT token is no longer valid.

**Solutions:**
```bash
# Check token expiration
ggen mcp auth decode-token "$TOKEN"

# Generate new token
NEW_TOKEN=$(ggen mcp auth generate-token --client-id "my-client")

# Configure auto-refresh
ggen mcp config set auth.jwt.auto_refresh=true
```

### Certificate Errors

**Problem:** mTLS connection fails with certificate errors.

**Solutions:**
```bash
# Verify certificate chain
openssl verify -CAfile ca.crt server.crt

# Check certificate expiration
openssl x509 -in server.crt -noout -dates

# Verify key matches certificate
openssl x509 -noout -modulus -in server.crt | openssl md5
openssl rsa -noout -modulus -in server.key | openssl md5

# Test connection with verbose output
curl -v --cert client.crt --key client.key \
  --cacert ca.crt https://localhost:8443/tools
```

## Next Steps

- **Configure server:** [How to Configure MCP Server Settings](configure-server.md)
- **Troubleshoot connections:** [How to Troubleshoot Connection Issues](troubleshoot-connection.md)
- **Bridge agents:** [How to Bridge A2A Agents as MCP Tools](bridge-agents.md)
- **Security guide:** [Security Best Practices](../explanations/security.md)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
