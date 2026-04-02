<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ChatmanGPT End-to-End Demo Specification](#chatmangpt-end-to-end-demo-specification)
  - [Demo Overview](#demo-overview)
    - [Input (What the Customer Describes)](#input-what-the-customer-describes)
  - [Demo Process (Live Execution)](#demo-process-live-execution)
    - [Step 1: ggen Compilation (10 minutes)](#step-1-ggen-compilation-10-minutes)
    - [Step 2: MCP Server Generated (Automatic)](#step-2-mcp-server-generated-automatic)
    - [Step 3: MCP Server Running (in-process)](#step-3-mcp-server-running-in-process)
    - [Step 4: Claude Client Makes Proposal Request](#step-4-claude-client-makes-proposal-request)
    - [Step 5: Tool Execution → Proposal Generation](#step-5-tool-execution-%E2%86%92-proposal-generation)
    - [Step 6: Proposal Output (What Claude Returns)](#step-6-proposal-output-what-claude-returns)
    - [Step 7: Customer Approval (Outside MCP)](#step-7-customer-approval-outside-mcp)
    - [Step 8: Executor Runs (Customer's CI/CD)](#step-8-executor-runs-customers-cicd)
    - [Step 9: Verification & Audit Trail](#step-9-verification--audit-trail)
  - [Demo Output Files (Deliverables)](#demo-output-files-deliverables)
    - [1. **MCP Server Package** (Production-Ready)](#1-mcp-server-package-production-ready)
    - [2. **Infrastructure Module** (Terraform)](#2-infrastructure-module-terraform)
    - [3. **CI Pipeline Configuration** (GitHub Actions)](#3-ci-pipeline-configuration-github-actions)
    - [4. **Receipt Chain** (Proofs)](#4-receipt-chain-proofs)
    - [5. **Documentation** (Generated)](#5-documentation-generated)
  - [Demo Narrative (What We Show)](#demo-narrative-what-we-show)
    - [Act 1: "Here's Your Ontology"](#act-1-heres-your-ontology)
    - [Act 2: "Watch It Compile"](#act-2-watch-it-compile)
    - [Act 3: "Here's a Proposal"](#act-3-heres-a-proposal)
    - [Act 4: "You Approve in CI"](#act-4-you-approve-in-ci)
    - [Act 5: "It Executes in Your Cloud"](#act-5-it-executes-in-your-cloud)
    - [Act 6: "The Proof"](#act-6-the-proof)
    - [Act 7: "Run It Again"](#act-7-run-it-again)
  - [Demo Success Criteria](#demo-success-criteria)
    - [Technical](#technical)
    - [Narrative](#narrative)
  - [Demo Assets](#demo-assets)
    - [Files to Generate](#files-to-generate)
    - [Files to Share](#files-to-share)
  - [Post-Demo Sales Acceleration](#post-demo-sales-acceleration)
    - [What Happens After](#what-happens-after)
  - [The Demo is the Product](#the-demo-is-the-product)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ChatmanGPT End-to-End Demo Specification

**Status**: Detailed design for canonical product demo
**Purpose**: Turn architecture from theoretical → inevitable
**Outcome**: Reproducible demo that becomes sales asset + onboarding accelerant + regression test

---

## Demo Overview

### Input (What the Customer Describes)

**1. Tiny Enterprise Ontology** (5 entities, 2 workflows)

```turtle
@prefix demo: <https://chatmangpt-demo.io/> .

demo:Company1 a ggen:Customer ;
  rdfs:label "Example Corp" ;
  ggen:hasEntity [
    a demo:Identity ;
    rdfs:label "Role" ;
    ggen:properties ("name" "permissions" "constraints")
  ] ;
  ggen:hasEntity [
    a demo:Workflow ;
    rdfs:label "ProvisionIdentity" ;
    ggen:steps ("createRole" "attachPolicy" "assumeRoleTest")
  ] ;
  ggen:hasEntity [
    a demo:Workflow ;
    rdfs:label "ConfigureEventFlow" ;
    ggen:steps ("createEventRule" "attachTarget" "testTrigger")
  ] ;
  ggen:hasEntity [
    a demo:Policy ;
    rdfs:label "SecurityPolicy" ;
    ggen:constraints ("noPublicAssumeRole" "encryptionRequired")
  ] ;
  ggen:hasEntity [
    a demo:EventRule ;
    rdfs:label "EventRule" ;
    ggen:properties ("name" "pattern" "target" "enabled")
  ] ;
  ggen:hasEntity [
    a demo:DataObject ;
    rdfs:label "EventMessage" ;
    ggen:properties ("source" "detailType" "payload" "timestamp")
  ] .
```

**2. AWS Artery Slice** (3 services, minimal viable integration)

```turtle
demo:AWSProvider a ggen:Provider ;
  rdfs:label "AWS Integration Layer" ;
  ggen:service [
    a demo:IAMService ;
    rdfs:label "Identity & Access Management" ;
    ggen:operations ("CreateRole" "PutRolePolicy" "GetRole") ;
    ggen:constraints ("NoBoundaryPolicy" "NoWildcardPrincipal")
  ] ;
  ggen:service [
    a demo:EventBridgeService ;
    rdfs:label "Event Routing & Delivery" ;
    ggen:operations ("PutRule" "PutTargets" "TestEventPattern") ;
    ggen:constraints ("EncryptionAtRest" "RegionRestriction")
  ] ;
  ggen:service [
    a demo:LambdaService ;
    rdfs:label "Serverless Compute" ;
    ggen:operations ("CreateFunction" "UpdateFunctionCode") ;
    ggen:constraints ("NoPublicEndpoint" "TimeoutLimit")
  ] .
```

**3. Stop-the-Line Guard Pack**

```turtle
demo:GuardPack a ggen:GuardSet ;
  ggen:guard [
    rdfs:label "NoPublicAssumeRole" ;
    ggen:query "SELECT ?role WHERE { ?role hasAssumePolicy ?policy. ?policy allowsPrincipal \"*\" }" ;
    ggen:failIf "query returns any results" ;
    ggen:message "Assume role policy cannot grant access to all principals"
  ] ;
  ggen:guard [
    rdfs:label "EncryptionRequired" ;
    ggen:query "SELECT ?rule WHERE { ?rule a EventBridgeRule. ?rule encryptionEnabled false }" ;
    ggen:failIf "query returns any results" ;
    ggen:message "EventBridge rules must have encryption enabled"
  ] ;
  ggen:guard [
    rdfs:label "RegionCompliance" ;
    ggen:query "SELECT ?resource WHERE { ?resource deployRegion ?region. NOT(?region IN (us-east-1, us-west-2)) }" ;
    ggen:failIf "query returns any results" ;
    ggen:message "Resources must deploy to approved regions only"
  ] .
```

---

## Demo Process (Live Execution)

### Step 1: ggen Compilation (10 minutes)

**Input**: Ontology + Guards + Provider constraints

**Process**:
```bash
$ ggen compile \
  --ontology /demo/ontology.ttl \
  --provider aws \
  --guards /demo/guards.ttl \
  --output /demo/generated
```

**What happens internally**:
1. Parse ontology (5 entities, 2 workflows)
2. Execute 6 extraction queries (Q1-Q6)
   - Q1: Entity extraction → 5 resources
   - Q2: Tool extraction → 2 tools
   - Q3: Workflow extraction → 2 prompts
   - Q4: Guard bindings → 3 guards per tool
   - Q5: Provider projection → IAM + EventBridge operations
   - Q6: Receipt generation → proof envelope
3. Generate Tera templates
4. Compile TypeScript/Node code
5. Emit receipts

**Output**: MCP Server package (ready to run)

---

### Step 2: MCP Server Generated (Automatic)

**Generated Files**:

```
/demo/generated/
├── mcp-server.ts          # Main MCP server
├── types.ts               # TypeScript definitions
├── resources.ts           # 5 resources (Identity, Role, Policy, EventRule, EventMessage)
├── tools.ts               # 2 tools (CreateIdentity, ConfigureEventFlow)
├── prompts.ts             # 2 prompts (ProvisionIdentity workflow, ConfigureEventFlow workflow)
├── guards.ts              # Guard evaluators
├── provider-plan.ts       # AWS operation planner
├── receipts.ts            # Receipt generator
├── package.json           # Dependencies
└── receipt-v1.json        # Proof envelope (signed)
```

**Server Capabilities**:

| Tool | Input | Output | Safety |
|------|-------|--------|--------|
| **CreateIdentity** | `{role: "AppTeamRole", region: "us-east-1"}` | Proposal | Guards: NoPublicAssumeRole, RegionCompliance |
| **ConfigureEventFlow** | `{ruleName: "ProcessOrders", pattern: {...}}` | Proposal | Guards: EncryptionRequired, RegionCompliance |

---

### Step 3: MCP Server Running (in-process)

**Start server**:
```bash
$ node /demo/generated/mcp-server.ts --port 3000
```

**Server listens** for Claude (or any client) to:
1. List available resources
2. Call tools with inputs
3. Run prompts
4. Get proposals

---

### Step 4: Claude Client Makes Proposal Request

**User prompt**:
```
I need to provision identity for the application team.
Use the CreateIdentity tool to propose this.
```

**Claude calls tool**:
```json
{
  "toolName": "CreateIdentity",
  "input": {
    "role": "AppTeamRole",
    "region": "us-east-1"
  }
}
```

---

### Step 5: Tool Execution → Proposal Generation

**What happens**:

```typescript
// Inside mcp-server.ts
async function createIdentity(input: CreateIdentityInput): Promise<Proposal> {
  // Step 1: Validate input against schema (Zod)
  const validated = CreateIdentitySchema.parse(input);

  // Step 2: Evaluate guards BEFORE any computation
  const guardResults = await evaluateGuards(validated, guardPack);
  if (guardResults.some(g => g.result === 'FAIL')) {
    return {
      status: 'proposed',
      message: `Cannot proceed: ${guardResults.filter(g => g.result === 'FAIL').map(g => g.message).join(', ')}`,
      guards: guardResults,
      proposal: null
    };
  }

  // Step 3: Build provider plan (what WOULD execute)
  const providerPlan = buildProviderPlan({
    provider: 'AWS',
    operations: [
      { op: 'CreateRole', requires: ['iam:CreateRole'], idempotent: true },
      { op: 'PutRolePolicy', requires: ['iam:PutRolePolicy'], idempotent: true },
      { op: 'GetRole', requires: ['iam:GetRole'], idempotent: true }
    ],
    constraints: [
      { type: 'RegionConstraint', value: 'us-east-1' },
      { type: 'NoWildcardPrincipals', value: true }
    ]
  });

  // Step 4: Serialize for receipt
  const proposalObject = {
    tool: 'CreateIdentity',
    input: validated,
    plan: providerPlan,
    guards: guardResults,
    status: 'proposed'
  };

  // Step 5: Generate receipt (proof of lawfulness)
  const receipt = generateReceipt({
    inputHash: sha256(JSON.stringify(validated)),
    proposalHash: sha256(JSON.stringify(proposalObject)),
    guards: guardResults.map(g => ({
      guard: g.name,
      result: g.result,
      proof: sha256(g.condition + JSON.stringify(g.evaluation))
    })),
    steps: [
      { mu: 'validate_input', status: 'PASS' },
      { mu: 'evaluate_guards', status: 'PASS' },
      { mu: 'build_provider_plan', status: 'PASS' },
      { mu: 'serialize_proposal', status: 'PASS' }
    ],
    timestamp: Date.now()
  });

  // Step 6: Return proposal (NEVER execute)
  return {
    tool: 'CreateIdentity',
    input: validated,
    plan: providerPlan,
    guards: guardResults,
    receipt: receipt,
    status: 'proposed',
    message: 'Review and approve to execute'
  };
}
```

---

### Step 6: Proposal Output (What Claude Returns)

```json
{
  "tool": "CreateIdentity",
  "input": {
    "role": "AppTeamRole",
    "region": "us-east-1"
  },
  "plan": {
    "provider": "AWS",
    "operations": [
      {
        "op": "CreateRole",
        "params": {
          "RoleName": "AppTeamRole",
          "AssumeRolePolicyDocument": "{...policy...}"
        },
        "requires": ["iam:CreateRole"],
        "idempotent": true
      },
      {
        "op": "PutRolePolicy",
        "params": {
          "RoleName": "AppTeamRole",
          "PolicyName": "AppTeamPolicy",
          "PolicyDocument": "{...policy...}"
        },
        "requires": ["iam:PutRolePolicy"],
        "idempotent": true
      }
    ],
    "constraints": [
      {
        "type": "RegionConstraint",
        "value": "us-east-1",
        "satisfied": true
      },
      {
        "type": "NoWildcardPrincipals",
        "value": true,
        "satisfied": true
      }
    ]
  },
  "guards": [
    {
      "name": "NoPublicAssumeRole",
      "condition": "AssumeRolePolicyDocument does not contain 'Principal: *'",
      "result": "PASS",
      "proof": "sha256(condition + evaluation) = 0x7f3c...",
      "message": "Assume role policy correctly restricts to named principals"
    },
    {
      "name": "RegionCompliance",
      "condition": "region in [us-east-1, us-west-2]",
      "result": "PASS",
      "proof": "sha256(...) = 0xa2e1...",
      "message": "Deployment region us-east-1 is approved"
    },
    {
      "name": "EncryptionRequired",
      "condition": "N/A (not applicable to IAM)",
      "result": "SKIP",
      "proof": "N/A",
      "message": "Guard does not apply to this tool"
    }
  ],
  "receipt": {
    "input_hash": "sha256(CreateIdentityInput) = 0x4d8f...",
    "proposal_hash": "sha256(ProposalObject) = 0xb2c1...",
    "guards": [
      { "guard": "NoPublicAssumeRole", "result": "PASS", "proof": "0x7f3c..." },
      { "guard": "RegionCompliance", "result": "PASS", "proof": "0xa2e1..." }
    ],
    "steps": [
      { "mu": "validate_input", "status": "PASS", "hash_after": "0x4d8f..." },
      { "mu": "evaluate_guards", "status": "PASS", "hash_after": "0x9c2e..." },
      { "mu": "build_provider_plan", "status": "PASS", "hash_after": "0xb2c1..." },
      { "mu": "serialize_proposal", "status": "PASS", "hash_after": "0xb2c1..." }
    ],
    "timestamp": 1705708800,
    "proof": "sha256(entire_receipt) = 0x3f1a...",
    "signature": "Ed25519(proof, signing_key) = 0xd4e7..."
  },
  "status": "proposed",
  "message": "Review and approve to execute. All guards passed. Ready for deployment."
}
```

---

### Step 7: Customer Approval (Outside MCP)

**Where this happens**: Customer's CI/CD or dashboard

**Approval workflow**:
1. Engineer reviews proposal
2. Notes: "Looks good, creating AppTeamRole in prod"
3. Clicks "Approve"
4. System creates GitHub PR with:
   - Proposal JSON (readable)
   - Receipt (proof)
   - Executor script (apply-proposal.sh)

**GitHub PR looks like**:

```
Title: Approve CreateIdentity Proposal for AppTeamRole

Body:
Tool: CreateIdentity
Input: {"role": "AppTeamRole", "region": "us-east-1"}

Operations to Execute:
1. CreateRole (RoleName: AppTeamRole) - iam:CreateRole
2. PutRolePolicy (PolicyName: AppTeamPolicy) - iam:PutRolePolicy

Guards Passed:
✓ NoPublicAssumeRole
✓ RegionCompliance

Receipt Proof: 0x3f1a...
```

---

### Step 8: Executor Runs (Customer's CI/CD)

**Executor** (runs in customer's CI/CD, uses their credentials):

```bash
#!/bin/bash

# Run with customer's AWS credentials (not vendor's)
export AWS_ROLE_ARN=$(assume-role customer-infra-role)

# Apply the approved proposal
chatmangpt-executor apply \
  --proposal /approvals/proposal-2026-01-19-001.json \
  --receipt /approvals/receipt-2026-01-19-001.json \
  --region us-east-1

# Verify execution
if [ $? -eq 0 ]; then
  aws iam get-role --role-name AppTeamRole
  echo "✓ Proposal executed successfully"
else
  echo "✗ Proposal execution failed, rolling back"
  chatmangpt-executor rollback --proposal-id 2026-01-19-001
fi
```

**What executor does**:
1. Reads approved proposal + receipt
2. Assumes customer's credentials
3. Executes operations in order (idempotent, resumable)
4. Logs all operations
5. Verifies constraints post-execution
6. Saves execution receipt (what actually happened vs what was proposed)

---

### Step 9: Verification & Audit Trail

**Execution Receipt** (what actually happened):

```json
{
  "proposal_id": "2026-01-19-001",
  "proposal_hash": "0xb2c1...",
  "execution_timestamp": 1705708900,
  "operations_executed": [
    {
      "op": "CreateRole",
      "status": "COMPLETED",
      "actual_arn": "arn:aws:iam::123456789012:role/AppTeamRole",
      "duration_ms": 245,
      "timestamp": 1705708810
    },
    {
      "op": "PutRolePolicy",
      "status": "COMPLETED",
      "policy_arn": "arn:aws:iam::123456789012:role/AppTeamRole/policy/AppTeamPolicy",
      "duration_ms": 112,
      "timestamp": 1705708825
    }
  ],
  "final_state": "SUCCESS",
  "execution_hash": "sha256(all_operations) = 0x7c2d...",
  "rollback_capable": true,
  "rollback_instructions": "Delete role + policy (order: policy first, then role)"
}
```

**Audit Trail**:
- Proposal PR → Approval comment → Execution logs → Final state
- All attached to receipt chain (immutable proof)
- Compliance can verify without trusting code

---

## Demo Output Files (Deliverables)

### 1. **MCP Server Package** (Production-Ready)
```
/generated/mcp-server/
├── src/
│   ├── server.ts          # MCP server entrypoint
│   ├── types.ts           # TypeScript definitions
│   ├── resources/         # Resource definitions
│   ├── tools/             # Tool implementations
│   ├── prompts/           # Prompt definitions
│   ├── guards/            # Guard evaluators
│   ├── providers/         # AWS provider adapter
│   └── receipts/          # Receipt generation
├── tests/
│   ├── tools.test.ts      # Tool proposal tests
│   └── guards.test.ts     # Guard evaluation tests
├── package.json
└── README.md
```

### 2. **Infrastructure Module** (Terraform)
```
/generated/infrastructure/
├── main.tf                # IAM roles, EventBridge rules
├── variables.tf           # Input variables
├── outputs.tf             # Exported values
├── terraform.tfvars       # Demo values
└── README.md
```

### 3. **CI Pipeline Configuration** (GitHub Actions)
```
/generated/.github/workflows/
├── generate-proposals.yml # On commit: run ggen, emit proposals
├── approve-proposal.yml   # On PR approval: create executor job
└── execute-proposal.yml   # On merge: run executor in prod
```

### 4. **Receipt Chain** (Proofs)
```
/generated/.ggen-receipts/
├── generation-2026-01-19.json     # Generation receipt
├── proposal-001-2026-01-19.json   # Proposal receipt
└── execution-001-2026-01-19.json  # Execution receipt
```

### 5. **Documentation** (Generated)
```
/generated/docs/
├── API.md                 # MCP tools + resources
├── WORKFLOW.md            # Guided workflows
├── ARCHITECTURE.md        # System design (generated from ontology)
└── RECEIPTS.md            # Receipt format + verification
```

---

## Demo Narrative (What We Show)

### Act 1: "Here's Your Ontology"
**Show**: 3 RDF files describing customer's domain (roles, workflows, policies)
**Point**: "This is all ChatmanGPT needs to know about your operations"

### Act 2: "Watch It Compile"
**Show**: Run `ggen compile`, output MCP server in 3 minutes
**Point**: "No code review. No meetings. Ontology → executable system."

### Act 3: "Here's a Proposal"
**Show**: Claude calls CreateIdentity tool, receives proposal (never executes)
**Point**: "See all the AWS operations this will do. See all the guards that passed. See the receipt proving it's safe."

### Act 4: "You Approve in CI"
**Show**: GitHub PR with proposal, engineer clicks "Approve"
**Point**: "Approval is a PR. Approval is auditable. Compliance can see what was approved and when."

### Act 5: "It Executes in Your Cloud"
**Show**: Executor runs in customer's CI/CD with customer's credentials
**Point**: "We never touched your cloud. You own the execution. You can rollback in 2 minutes if something goes wrong."

### Act 6: "The Proof"
**Show**: Receipt chain showing hashes, proofs, signatures
**Point**: "Here's cryptographic proof this proposal was lawful, reproducible, and correctly executed. This is insurance-understandable."

### Act 7: "Run It Again"
**Show**: Run ggen compile again with same ontology, get identical proposal + receipt
**Point**: "Determinism. Same inputs always produce same outputs. Try this with an integrator."

---

## Demo Success Criteria

### Technical
- [ ] ggen compiles without errors (5 min)
- [ ] MCP server starts and responds to requests
- [ ] Tools generate proposals (not execute)
- [ ] Guards evaluate correctly (all pass for demo scenario)
- [ ] Receipt hashes are reproducible (run 3x, same hashes)
- [ ] Executor applies proposal without customer credentials in MCP
- [ ] Rollback works (restore previous state)

### Narrative
- [ ] Each act takes <5 minutes
- [ ] No jargon (translate all technical concepts)
- [ ] Emphasize safety ("we never execute", "you own approval", "proof is cryptographic")
- [ ] End on competitive point (vs integrators: 10x cheaper, 3x faster, 100% reversible)

---

## Demo Assets

### Files to Generate
1. **demo-ontology.ttl** (ready-made for fast demo)
2. **demo-guards.ttl** (3 common guards)
3. **generated/** (output from ggen compile)
4. **demo-script.md** (talking points for each act)
5. **demo-metrics.json** (speed measurements: ggen compile time, proposal generation time, etc.)

### Files to Share
1. **MCP server** (GitHub repo, deployable)
2. **Architecture diagrams** (generated from ontology)
3. **Receipts** (example proof chain)
4. **Comparison table** (ChatmanGPT vs integrators)

---

## Post-Demo Sales Acceleration

### What Happens After
1. **Customer asks**: "Can we run this on our ontology?"
2. **Sales says**: "Yes, but let's start with your domains one at a time"
3. **Onboarding**:
   - Week 1: Ontology interview (1-2 hours)
   - Week 2: First MCP generated
   - Week 3: Guards configured + tested
   - Week 4: Executor integrated into customer's CI/CD
   - Week 5: Production pilot (one workflow)

4. **From pilot to contract**:
   - Customer sees speed (4 weeks vs 6 months)
   - Customer sees determinism (reproducible)
   - Customer sees safety (all receipted)
   - Customer signs $100-150k contract (Year 1)

---

## The Demo is the Product

This demo becomes:
- **Sales Demo** (wins deals)
- **Onboarding Accelerant** (speeds implementation)
- **Regression Test** (ensures quality)
- **Investor Weapon** (proves viability)
- **Marketing Asset** (video, blog, webinar)

Once the demo exists, closing deals becomes inevitable because:
1. It proves the technology works
2. It proves the safety model works
3. It proves the speed advantage (quantified)
4. It removes all technical risk from customer evaluation

**Status**: Ready to implement

**Next**: Code the demo (2-3 weeks) → Get first customer → Prove unit economics
