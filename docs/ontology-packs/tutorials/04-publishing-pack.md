# Tutorial: Publishing to the Marketplace

**Goal:** Publish your ontology pack to the ggen marketplace for others to use.

**What you'll learn:**
- Marketplace requirements and guidelines
- Signing and verifying packs
- Publishing workflow
- Versioning and updates
- Marketing your pack

**Prerequisites:**
- Completed [Building Your Own Pack](03-building-custom-pack.md)
- Created ggen marketplace account
- Pack thoroughly tested

**Time:** 30 minutes

---

## Step 1: Prepare Pack for Publishing

### Create README.md

Create `my-company-ontology/README.md`:

```markdown
# MyCompany Project Management Ontology Pack

Internal ontology for project management, tasks, teams, and milestones.

## Features

- 4 core types: Project, Task, Team, Milestone
- 15 properties covering all project management needs
- Templates for TypeScript and Python
- Fully documented with examples

## Installation

\`\`\`bash
ggen ontology install my-company-ontology
\`\`\`

## Quick Start

Generate TypeScript types:

\`\`\`bash
ggen ontology generate my-company-ontology \
  --template typescript \
  --output ./src/types
\`\`\`

## Usage Example

\`\`\`typescript
import { Project, Task } from './types';

const project: Project = {
  "@type": "Project",
  projectId: "PROJ-001",
  projectName: "New Website",
  startDate: "2025-01-01T00:00:00Z"
};
\`\`\`

## Documentation

See [docs/](docs/) for detailed documentation.

## License

MIT
```

### Create CHANGELOG.md

```markdown
# Changelog

## [1.0.0] - 2025-11-18

### Added
- Initial release
- Project, Task, Team, Milestone types
- TypeScript template
- Python template
- Complete documentation
```

### Add LICENSE

```
MIT License

Copyright (c) 2025 MyCompany

Permission is hereby granted, free of charge, to any person obtaining a copy...
```

---

## Step 2: Validate Pack Quality

Run quality checks:

```bash
ggen ontology validate ./my-company-ontology --strict
```

**Checks performed:**
- ✓ pack.yaml schema validation
- ✓ Ontology syntax validation (RDF)
- ✓ Template syntax validation
- ✓ SPARQL query validation
- ✓ Required files present
- ✓ Documentation completeness
- ✓ Version format
- ✓ License declared

Fix any issues before proceeding.

---

## Step 3: Sign Your Pack

Generate signing key (first time only):

```bash
ggen keys generate --name "mycompany-dev"
```

**Output:**
```
Generated key pair:
  Public key: ~/.ggen/keys/mycompany-dev.pub
  Private key: ~/.ggen/keys/mycompany-dev.key

⚠️  IMPORTANT: Keep your private key secure!
```

Sign the pack:

```bash
ggen ontology sign ./my-company-ontology \
  --key ~/.ggen/keys/mycompany-dev.key \
  --output ./my-company-ontology-1.0.0.signed.gpack
```

**What happens:**
1. Pack is hashed using SHA-256
2. Hash is signed with your private key
3. Signature is embedded in `.gpack` file
4. Public key fingerprint is included

Users can verify your pack with your public key.

---

## Step 4: Test in Isolated Environment

Create a test project:

```bash
mkdir test-install
cd test-install
ggen ontology install ../my-company-ontology-1.0.0.signed.gpack
```

Verify installation:

```bash
ggen ontology info my-company-ontology
ggen ontology generate my-company-ontology --template typescript --output ./test
```

Test the generated code:

```bash
cd test
npm init -y
npm install --save-dev typescript @types/node
npx tsc --init
# Create test.ts and run it
```

---

## Step 5: Create Marketplace Account

Visit: https://marketplace.ggen.dev

1. Click "Sign Up"
2. Verify email
3. Complete profile:
   - Developer name
   - Organization (optional)
   - Website
   - GitHub profile

4. Add public signing key:
   ```bash
   cat ~/.ggen/keys/mycompany-dev.pub
   ```
   Paste into "Developer Keys" section

---

## Step 6: Publish to Marketplace

### Via CLI (Recommended)

```bash
ggen marketplace publish ./my-company-ontology-1.0.0.signed.gpack \
  --token $GGEN_MARKETPLACE_TOKEN \
  --category "project-management" \
  --tags "typescript,python,internal" \
  --visibility "public"
```

**Options:**
- `--category`: Primary category (required)
- `--tags`: Comma-separated tags
- `--visibility`: `public` or `private`
- `--pricing`: `free` or price in USD (e.g., `9.99`)

**Expected output:**
```
Publishing ontology pack...
✓ Uploaded pack (15.2 KB)
✓ Verified signature
✓ Validated metadata
✓ Processed templates
✓ Indexed for search

Published: https://marketplace.ggen.dev/packs/my-company-ontology

Status: Under review (usually 24-48 hours)
```

### Via Web Interface

1. Go to https://marketplace.ggen.dev/publish
2. Click "Upload Pack"
3. Select `my-company-ontology-1.0.0.signed.gpack`
4. Fill in metadata:
   - **Category:** Project Management
   - **Tags:** typescript, python, internal
   - **Pricing:** Free
   - **Visibility:** Public
5. Click "Submit for Review"

---

## Step 7: Monitor Review Process

Check status:

```bash
ggen marketplace status my-company-ontology
```

**Review criteria:**
- Code quality and best practices
- Documentation completeness
- Template correctness
- Security (no malicious code)
- License compliance
- Duplicate detection

**Timeline:**
- Automated checks: ~5 minutes
- Manual review: 24-48 hours
- Approval notification via email

---

## Step 8: Post-Publication

Once approved:

### Update pack.yaml

Add marketplace metadata:

```yaml
marketplace:
  pack_id: "my-company-ontology"
  published_at: "2025-11-18T15:30:00Z"
  url: "https://marketplace.ggen.dev/packs/my-company-ontology"
```

### Share Your Pack

```bash
ggen marketplace share my-company-ontology \
  --social twitter,linkedin \
  --message "Just published my ontology pack for project management!"
```

### Monitor Analytics

```bash
ggen marketplace analytics my-company-ontology
```

**Metrics shown:**
- Downloads per day/week/month
- Active installations
- Star ratings
- Template usage breakdown
- User feedback

---

## Step 9: Publish Updates

Make changes to your pack:

1. Update version in `pack.yaml`:
   ```yaml
   version: "1.1.0"
   ```

2. Update CHANGELOG.md:
   ```markdown
   ## [1.1.0] - 2025-12-01

   ### Added
   - New Sprint type
   - Dependencies between tasks

   ### Fixed
   - TypeScript type guard edge case
   ```

3. Sign new version:
   ```bash
   ggen ontology sign ./my-company-ontology \
     --key ~/.ggen/keys/mycompany-dev.key \
     --output ./my-company-ontology-1.1.0.signed.gpack
   ```

4. Publish update:
   ```bash
   ggen marketplace publish ./my-company-ontology-1.1.0.signed.gpack \
     --update
   ```

Users will be notified of the update.

---

## Step 10: Deprecate Old Versions

Deprecate old versions:

```bash
ggen marketplace deprecate my-company-ontology --version "1.0.0" \
  --reason "Security fix in 1.1.0"
```

Users on 1.0.0 will see:
```
⚠️  Warning: my-company-ontology@1.0.0 is deprecated
   Reason: Security fix in 1.1.0
   Run: ggen ontology upgrade my-company-ontology
```

---

## Marketing Your Pack

### 1. Create Landing Page

Add to your website:
- Overview of the ontology
- Installation instructions
- Usage examples
- Link to marketplace

### 2. Write Blog Post

Topics:
- Why you created the pack
- Problem it solves
- How it works
- Real-world examples

### 3. Share on Social Media

Templates:
```
🚀 Just published my project management ontology pack on @ggen!

✅ 4 core types
✅ TypeScript + Python templates
✅ Production-ready

Install: ggen ontology install my-company-ontology

https://marketplace.ggen.dev/packs/my-company-ontology
```

### 4. Create Video Tutorial

Topics:
- Installation walkthrough
- Code generation demo
- Real project integration
- Tips and tricks

### 5. Engage with Community

- Answer questions on discussions
- Help users with issues
- Accept pull requests
- Share user success stories

---

## Best Practices

1. **Version responsibly:** Follow semantic versioning
2. **Test thoroughly:** Every release should be tested
3. **Document changes:** Keep CHANGELOG.md updated
4. **Respond to issues:** Engage with your users
5. **Security first:** Never include secrets or credentials
6. **License clearly:** Use standard open source licenses
7. **Examples matter:** Provide real-world usage examples

---

## What You Learned

- ✅ Preparing pack for publication
- ✅ Signing packs with cryptographic keys
- ✅ Publishing to marketplace via CLI and web
- ✅ Monitoring review process
- ✅ Publishing updates and deprecations
- ✅ Marketing your pack

---

## Next Steps

- **Advanced generation:** [How to: Customize Code Generation](../how-to/customize-generation.md)
- **Composition:** [How to: Compose Multiple Ontologies](../how-to/compose-ontologies.md)
- **Architecture deep-dive:** [Architecture: Marketplace](../explanations/marketplace-architecture.md)

---

## Troubleshooting

**Signature verification fails?**
- Ensure public key is added to marketplace account
- Check key hasn't been regenerated
- Verify `.gpack` file isn't corrupted

**Review rejected?**
- Read rejection reason carefully
- Fix issues and resubmit
- Contact support if unclear: support@ggen.dev

**Can't update pack?**
- Ensure you're signed in as pack owner
- Check version number is higher than current
- Use `--force` flag if absolutely necessary
