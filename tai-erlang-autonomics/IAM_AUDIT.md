# IAM Audit Guide - erlmcp

## Quick Reference

### Who Has What Permissions?

**Service Accounts**:
- `taiea-sa`: Cloud Run application (read/write Firestore, logging, metrics)
- `taiea-admin`: Infrastructure admin (deploy, manage IAM, manage Firestore)
- `taiea-api-client`: External API clients (invoke Cloud Run, read/write Firestore)
- `taiea-billing`: Billing system (invoke Cloud Run, read/write billing data)

**User Groups**:
- Project Owners: Full access (use sparingly)
- DevOps Engineers: Cloud Run, Firestore admin access
- Application Support: Cloud Run viewer, logging reader

### Permission Matrix

| Role | Service | Action | Permission |
|------|---------|--------|-----------|
| taiea-sa | Cloud Run | Execute | (implicit - runs service) |
| taiea-sa | Firestore | Read/Write documents | `datastore.user` |
| taiea-sa | Logging | Write logs | `logging.logWriter` |
| taiea-admin | Cloud Run | Deploy revisions | `run.admin` |
| taiea-admin | Firestore | Manage databases | `datastore.admin` |
| taiea-admin | IAM | Grant roles | `resourcemanager.projectIamAdmin` |
| taiea-api-client | Cloud Run | Invoke service | `run.invoker` |
| taiea-api-client | Firestore | Read/Write documents | `datastore.user` |
| taiea-billing | Cloud Run | Invoke service | `run.invoker` |
| taiea-billing | Firestore | Read/Write documents | `datastore.user` |
| taiea-billing | BigQuery | Write events | `bigquery.dataEditor` |

## Granting & Revoking Access

### Grant Cloud Run Invoker Role

```bash
# Grant to a service account
gcloud run services add-iam-policy-binding taiea \
  --region=us-central1 \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/run.invoker"

# Grant to a user
gcloud run services add-iam-policy-binding taiea \
  --region=us-central1 \
  --member="user:engineer@company.com" \
  --role="roles/run.invoker"

# Grant to all authenticated users
gcloud run services add-iam-policy-binding taiea \
  --region=us-central1 \
  --member="allAuthenticatedUsers" \
  --role="roles/run.invoker"
```

### Grant Firestore Access

```bash
# Grant read/write access (Firestore User)
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/datastore.user"

# Grant admin access (Firestore Admin)
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:taiea-admin@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/datastore.admin"
```

### Grant Custom Tenant Isolation Role

```bash
# Create custom role (already done in terraform)
gcloud iam roles create tenantFirestoreReader \
  --project=PROJECT_ID \
  --title="Tenant Firestore Reader" \
  --description="Read-only access to tenant-scoped Firestore documents" \
  --permissions="datastore.databases.get,datastore.entities.get,datastore.entities.list"

# Grant custom role
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="projects/PROJECT_ID/roles/tenantFirestoreReader"
```

### Revoke Access

```bash
# Revoke Cloud Run invoker role
gcloud run services remove-iam-policy-binding taiea \
  --region=us-central1 \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/run.invoker"

# Revoke project-level role
gcloud projects remove-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/datastore.user"
```

## Emergency Access Procedures

### Scenario: Need immediate Cloud Run access

**Steps**:
1. Verify the user/service account exists
2. Grant `roles/run.invoker` IAM binding
3. Document the grant with ticket number
4. Set expiration reminder (should be temporary)

**Example - Grant temporary access for 7 days**:
```bash
# Create custom role with condition
gcloud iam roles create temporaryCloudRunInvoker \
  --project=PROJECT_ID \
  --title="Temporary Cloud Run Invoker" \
  --permissions="run.routes.invoke"

# Grant with time condition (if supported)
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:temp-sa@PROJECT_ID.iam.gserviceaccount.com" \
  --role="projects/PROJECT_ID/roles/temporaryCloudRunInvoker" \
  --condition="resource.matchTag('env', 'temp') && api.getAttribute('auth.decision', 'ALLOW') && request.time < timestamp('2024-02-26T00:00:00Z')"
```

### Scenario: Service account compromised

**Immediate steps**:
1. Disable service account immediately
2. Revoke all active keys
3. List all resources owned by the account
4. Audit recent activities in Cloud Audit Logs
5. Rotate credentials for accounts that this service account authenticated to

**Commands**:
```bash
# Disable service account
gcloud iam service-accounts disable taiea-api-client@PROJECT_ID.iam.gserviceaccount.com

# List and delete keys
gcloud iam service-accounts keys list \
  --iam-account=taiea-api-client@PROJECT_ID.iam.gserviceaccount.com

gcloud iam service-accounts keys delete KEY_ID \
  --iam-account=taiea-api-client@PROJECT_ID.iam.gserviceaccount.com

# Re-enable after cleanup
gcloud iam service-accounts enable taiea-api-client@PROJECT_ID.iam.gserviceaccount.com

# Create new key
gcloud iam service-accounts keys create credentials/taiea-api-client-key.json \
  --iam-account=taiea-api-client@PROJECT_ID.iam.gserviceaccount.com
```

### Scenario: Need to audit who accessed a specific resource

**View recent access logs**:
```bash
# View Firestore access logs
gcloud logging read \
  'resource.type="cloud_firestore_database" AND protoPayload.methodName="google.datastore.v1.Datastore.Lookup"' \
  --limit=50 \
  --format=json \
  --project=PROJECT_ID

# View Cloud Run invocation logs
gcloud logging read \
  'resource.type="cloud_run_managed_environment"' \
  --limit=50 \
  --format=json \
  --project=PROJECT_ID

# Filter by specific service account
gcloud logging read \
  'protoPayload.authenticationInfo.principalEmail="taiea-api-client@PROJECT_ID.iam.gserviceaccount.com"' \
  --limit=50 \
  --format=json \
  --project=PROJECT_ID
```

## Audit Trail Location

### Cloud Audit Logs

**Location**: Cloud Logging console at https://console.cloud.google.com/logs

**Types**:
- **Admin Activity**: IAM changes, resource creation/deletion
- **Data Access**: Firestore reads/writes (if enabled)
- **System Events**: GCP internal activities

**Query for recent IAM changes**:
```bash
gcloud logging read \
  'protoPayload.methodName=~".*SetIamPolicy"' \
  --limit=100 \
  --format="table(timestamp, protoPayload.authenticationInfo.principalEmail, protoPayload.methodName, protoPayload.resourceName)" \
  --project=PROJECT_ID
```

### Firestore Audit Logs

**Enabled via**: Firestore > Settings > Audit Logs tab

**Queries**:
```bash
# View all Firestore operations
gcloud logging read \
  'resource.type="cloud_firestore_database"' \
  --limit=100 \
  --project=PROJECT_ID

# View specific document access
gcloud logging read \
  'resource.type="cloud_firestore_database" AND protoPayload.resourceName=~".*receipts.*"' \
  --limit=50 \
  --project=PROJECT_ID

# View by service account
gcloud logging read \
  'resource.type="cloud_firestore_database" AND protoPayload.authenticationInfo.principalEmail="taiea-api-client@PROJECT_ID.iam.gserviceaccount.com"' \
  --limit=50 \
  --project=PROJECT_ID
```

### Cloud Run Access Logs

**Enabled via**: Cloud Run > Service > Logging

**View logs**:
```bash
gcloud logging read \
  'resource.type="cloud_run_managed_environment" AND resource.labels.service_name="taiea"' \
  --limit=100 \
  --format=json \
  --project=PROJECT_ID
```

### Service Account Key Audit Trail

**Check key creation/deletion history**:
```bash
# View service account key operations
gcloud logging read \
  'protoPayload.methodName="google.iam.admin.v1.CreateServiceAccountKey" OR protoPayload.methodName="google.iam.admin.v1.DeleteServiceAccountKey"' \
  --limit=50 \
  --format="table(timestamp, protoPayload.authenticationInfo.principalEmail, protoPayload.methodName)" \
  --project=PROJECT_ID
```

## Regular Audit Tasks

### Weekly

- [ ] Review new IAM bindings: `gcloud projects get-iam-policy PROJECT_ID --format=json | jq '.bindings[] | select(.members | any(. | startswith("user:"))) | {role: .role, members: .members}'`
- [ ] Check for failed authentication attempts in logs
- [ ] Review Cloud Run invocation counts

### Monthly

- [ ] Audit service account permissions
- [ ] Check service account key age (rotate if > 90 days)
- [ ] Review Cloud Audit Logs for suspicious activity
- [ ] Verify no overprivileged accounts (Owner, Editor roles)

### Quarterly

- [ ] Full IAM policy review
- [ ] Service account rotation (create new, revoke old)
- [ ] Update documentation for access procedures
- [ ] Security training for team

### Annually

- [ ] Comprehensive security assessment
- [ ] Third-party penetration test
- [ ] Compliance audit (SOC2, etc.)
- [ ] Disaster recovery drill

## Automated Audit Checks

**GitHub Action**: `.github/workflows/audit-iam.yml`

**Runs**: Every Monday at 09:00 UTC

**Checks**:
1. Lists all IAM bindings
2. Flags Owner/Editor roles
3. Reviews service account keys
4. Detects new administrators
5. Checks Cloud Run access control
6. Exports audit trail

**Artifacts**: `iam-audit-reports/` (90-day retention)

## Compliance Requirements

### SOC 2 Controls

- **CC6.1**: IAM policy in place ✓
- **CC6.2**: Access changes logged ✓
- **CC6.3**: Unauthorized access prevented ✓
- **CC7.2**: System monitoring enabled ✓

### HIPAA (if applicable)

- Minimum necessary principle ✓ (principle of least privilege)
- Audit controls enabled ✓ (Cloud Audit Logs)
- Encryption in transit ✓ (HTTPS/TLS)

### GDPR (if applicable)

- Access control logged ✓
- Audit trail maintained ✓
- Data processing agreements in place

## Troubleshooting Access Issues

### Problem: "403 Unauthorized" when invoking Cloud Run

**Diagnosis**:
```bash
# Check if service account has run.invoker role
gcloud run services get-iam-policy taiea \
  --region=us-central1 \
  --format="table(bindings.role, bindings.members[])"

# Check service account IAM roles
gcloud projects get-iam-policy PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --format="table(bindings.role)"
```

**Fix**:
```bash
gcloud run services add-iam-policy-binding taiea \
  --region=us-central1 \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/run.invoker"
```

### Problem: "403 Forbidden" accessing Firestore

**Diagnosis**:
```bash
# Check Firestore permissions
gcloud projects get-iam-policy PROJECT_ID \
  --flatten="bindings[].members" \
  --filter="bindings.members:serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --format="table(bindings.role)"

# Check Firestore security rules
gcloud firestore databases describe --database="(default)" \
  --format="value(firestoreConfig.rules)"
```

**Fix**:
```bash
# Grant Firestore user role
gcloud projects add-iam-policy-binding PROJECT_ID \
  --member="serviceAccount:taiea-api-client@PROJECT_ID.iam.gserviceaccount.com" \
  --role="roles/datastore.user"

# Update Firestore security rules to allow access
```

### Problem: "401 Unauthorized" with expired token

**Diagnosis**:
```bash
# Check token expiration
gcloud auth print-access-token | jq -R 'split(".")[1] | @base64d | fromjson | .exp' | date -f -

# Or decode manually
TOKEN=$(gcloud auth print-access-token)
echo "$TOKEN" | cut -d. -f2 | base64 -d | jq '.exp'
```

**Fix**:
```bash
# Request new token
gcloud auth application-default login

# Or for service account
gcloud auth activate-service-account --key-file=credentials/taiea-api-client-key.json
```

## References

- [GCP IAM Best Practices](https://cloud.google.com/iam/docs/best-practices)
- [Cloud Run Security](https://cloud.google.com/run/docs/securing/managing-access)
- [Firestore Security Rules](https://firebase.google.com/docs/firestore/security/get-started)
- [GCP Audit Logs](https://cloud.google.com/logging/docs/audit)
