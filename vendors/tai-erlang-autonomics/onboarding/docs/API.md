# Onboarding Platform API Reference

Comprehensive REST API documentation for the value-based pricing onboarding platform.

## Base URL

```
https://api.onboarding.io/api/v1
```

## Authentication

All endpoints require JWT Bearer token authentication:

```bash
Authorization: Bearer <jwt_token>
```

### Login

```http
POST /auth/login
Content-Type: application/json

{
  "email": "user@company.com",
  "password": "securepassword"
}

Response: 200 OK
{
  "success": true,
  "data": {
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
    "userId": "550e8400-e29b-41d4-a716-446655440000",
    "email": "user@company.com",
    "role": "CUSTOMER_ADMIN"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Logout

```http
POST /auth/logout
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": { "message": "Logged out successfully" },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Refresh Token

```http
POST /auth/refresh
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": {
    "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Customers

### Create Customer

```http
POST /customers
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "name": "Acme Corporation",
  "email": "contact@acme.com"
}

Response: 201 Created
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "name": "Acme Corporation",
    "email": "contact@acme.com",
    "status": "ONBOARDING",
    "createdAt": "2024-01-25T10:00:00Z",
    "updatedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Get Customer

```http
GET /customers/:id
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "name": "Acme Corporation",
    "email": "contact@acme.com",
    "status": "ONBOARDING",
    "createdAt": "2024-01-25T10:00:00Z",
    "updatedAt": "2024-01-25T10:00:00Z",
    "setupSteps": [...],
    "valueDefinitions": [...],
    "approvals": [...]
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Setup Wizard

### Step 1: API Integration

```http
POST /customers/:id/setup/step1
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "apiKey": "sk_live_...",
  "webhookUrl": "https://webhook.acme.com/pricing"
}

Response: 200 OK
{
  "success": true,
  "data": {
    "step": 1,
    "title": "API Integration",
    "status": "COMPLETED",
    "completedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Step 2: Data Source Connection

```http
POST /customers/:id/setup/step2
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "dataSource": "salesforce",
  "credentials": {
    "instanceUrl": "https://acme.salesforce.com",
    "clientId": "...",
    "clientSecret": "..."
  }
}

Response: 200 OK
{
  "success": true,
  "data": {
    "step": 2,
    "title": "Data Source Connection",
    "status": "COMPLETED",
    "completedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Step 3: Measurement Configuration

```http
POST /customers/:id/setup/step3
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "metrics": [
    {
      "name": "Revenue Impact",
      "unit": "USD",
      "dataSource": "salesforce",
      "refreshInterval": 3600
    }
  ]
}

Response: 200 OK
{
  "success": true,
  "data": {
    "step": 3,
    "title": "Measurement Configuration",
    "status": "COMPLETED",
    "completedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Step 4: Baseline Establishment

```http
POST /customers/:id/setup/step4
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "baselineValue": 1000000,
  "baselinePeriod": "2024-01",
  "validationMethod": "manual_review"
}

Response: 200 OK
{
  "success": true,
  "data": {
    "step": 4,
    "title": "Baseline Establishment",
    "status": "COMPLETED",
    "completedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Step 5: Testing & Validation

```http
POST /customers/:id/setup/step5
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "testPeriod": 7,
  "readinessChecklist": {
    "dataFlowVerified": true,
    "measurementAccurate": true,
    "systemsIntegrated": true
  }
}

Response: 200 OK
{
  "success": true,
  "data": {
    "step": 5,
    "title": "Testing & Validation",
    "status": "COMPLETED",
    "completedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Value Definitions

### Create Value Definition

```http
POST /customers/:id/value-definitions
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "name": "Customer Revenue Impact",
  "description": "Total incremental revenue generated through our platform",
  "metrics": [
    {
      "name": "Revenue Impact",
      "unit": "USD",
      "description": "Incremental revenue from platform",
      "dataSource": "salesforce",
      "refreshInterval": 3600
    },
    {
      "name": "Cost Savings",
      "unit": "USD",
      "description": "Operational cost reductions",
      "dataSource": "accounting_system",
      "refreshInterval": 86400
    }
  ],
  "baselineValue": 1000000,
  "targetValue": 1500000,
  "scoringModel": "LINEAR"
}

Response: 201 Created
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440001",
    "customerId": "550e8400-e29b-41d4-a716-446655440000",
    "name": "Customer Revenue Impact",
    "status": "ACTIVE",
    "metrics": [...],
    "baselineValue": 1000000,
    "targetValue": 1500000,
    "createdAt": "2024-01-25T10:00:00Z",
    "updatedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Get Value Definitions

```http
GET /customers/:id/value-definitions
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440001",
      "customerId": "550e8400-e29b-41d4-a716-446655440000",
      "name": "Customer Revenue Impact",
      "status": "ACTIVE",
      ...
    }
  ],
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Update Value Definition

```http
PUT /customers/:id/value-definitions/:definitionId
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "targetValue": 2000000,
  "status": "ACTIVE"
}

Response: 200 OK
{
  "success": true,
  "data": {...},
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Receipts & Validation

### Get Receipts

```http
GET /customers/:id/receipts?type=VALUE_CALCULATION&limit=50
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": [
    {
      "id": "550e8400-e29b-41d4-a716-446655440002",
      "customerId": "550e8400-e29b-41d4-a716-446655440000",
      "type": "VALUE_CALCULATION",
      "hash": "a1b2c3d4e5f6...",
      "signature": "sig_...",
      "timestamp": "2024-01-25T10:00:00Z",
      "verifiedAt": "2024-01-25T10:00:01Z"
    }
  ],
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Validate Receipt

```http
POST /customers/:id/receipts/validate
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "receiptId": "550e8400-e29b-41d4-a716-446655440002"
}

Response: 200 OK
{
  "success": true,
  "data": {
    "receiptId": "550e8400-e29b-41d4-a716-446655440002",
    "isValid": true,
    "chainValid": true,
    "signatureValid": true,
    "verifiedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Approvals

### Create Approval

```http
POST /customers/:id/approvals
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "stage": "FINANCE_REVIEW",
  "approvers": [
    {
      "name": "Jane Doe",
      "email": "jane@acme.com",
      "role": "CFO"
    },
    {
      "name": "John Smith",
      "email": "john@acme.com",
      "role": "FINANCE_MANAGER"
    }
  ]
}

Response: 201 Created
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440003",
    "customerId": "550e8400-e29b-41d4-a716-446655440000",
    "stage": "FINANCE_REVIEW",
    "status": "PENDING",
    "approvers": [...],
    "createdAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Approve

```http
PUT /customers/:id/approvals/:approvalId/approve
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "comments": "Verified measurement methodology and accuracy. Approved for go-live."
}

Response: 200 OK
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440003",
    "status": "APPROVED",
    "approvedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Reject

```http
PUT /customers/:id/approvals/:approvalId/reject
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "reason": "Measurement methodology needs revision",
  "comments": "Please review the cost allocation formula"
}

Response: 200 OK
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440003",
    "status": "REJECTED",
    "rejectedAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Dashboard

### Get Dashboard Summary

```http
GET /customers/:id/dashboard/summary
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": {
    "customerId": "550e8400-e29b-41d4-a716-446655440000",
    "status": "APPROVAL_PENDING",
    "setupProgress": 100,
    "setupSteps": [...],
    "currentValue": 1250000,
    "targetValue": 1500000,
    "valueChangePercentage": 25.5,
    "measurementAccuracy": 92.5,
    "approvalsStatus": {
      "FINANCE_REVIEW": true,
      "TECHNICAL_REVIEW": false
    },
    "lastMeasurementAt": "2024-01-25T09:00:00Z",
    "nextMeasurementAt": "2024-01-26T09:00:00Z",
    "activeAlerts": [],
    "openTickets": 2
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Get Value Trend

```http
GET /customers/:id/dashboard/value-trend?period=30d
Authorization: Bearer <jwt_token>

Response: 200 OK
{
  "success": true,
  "data": [
    {
      "timestamp": "2024-01-01T00:00:00Z",
      "value": 1000000,
      "baseline": 1000000,
      "target": 1500000
    },
    {
      "timestamp": "2024-01-15T00:00:00Z",
      "value": 1125000,
      "baseline": 1000000,
      "target": 1500000
    },
    {
      "timestamp": "2024-01-25T00:00:00Z",
      "value": 1250000,
      "baseline": 1000000,
      "target": 1500000
    }
  ],
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Support Tickets

### Create Ticket

```http
POST /customers/:id/tickets
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "title": "Question about revenue calculation",
  "description": "How are recurring subscriptions counted in the revenue metric?",
  "category": "FINANCIAL",
  "priority": "HIGH"
}

Response: 201 Created
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440004",
    "customerId": "550e8400-e29b-41d4-a716-446655440000",
    "title": "Question about revenue calculation",
    "status": "OPEN",
    "priority": "HIGH",
    "category": "FINANCIAL",
    "messages": [],
    "createdAt": "2024-01-25T10:00:00Z"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### Add Message to Ticket

```http
POST /customers/:id/tickets/:ticketId/reply
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "message": "Recurring subscriptions are included at full annual value when the subscription is active."
}

Response: 201 Created
{
  "success": true,
  "data": {
    "id": "550e8400-e29b-41d4-a716-446655440005",
    "ticketId": "550e8400-e29b-41d4-a716-446655440004",
    "author": "support@onboarding.io",
    "message": "Recurring subscriptions are included at full annual value...",
    "createdAt": "2024-01-25T10:00:10Z"
  },
  "timestamp": "2024-01-25T10:00:10Z"
}
```

## Go-Live Switch

### Initiate Go-Live

```http
POST /customers/:id/go-live
Authorization: Bearer <jwt_token>
Content-Type: application/json

{
  "estimatedLiveDate": "2024-02-01T00:00:00Z"
}

Response: 200 OK
{
  "success": true,
  "data": {
    "customerId": "550e8400-e29b-41d4-a716-446655440000",
    "requestedAt": "2024-01-25T10:00:00Z",
    "requestedBy": "user@acme.com",
    "safetyChecks": {
      "dataFlowVerified": true,
      "measurementAccurate": true,
      "systemsIntegrated": true,
      "approvalsComplete": true
    },
    "readinessScore": 95,
    "estimatedLiveDate": "2024-02-01T00:00:00Z",
    "status": "PENDING"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Error Responses

### 400 Bad Request

```json
{
  "success": false,
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Invalid request data",
    "details": [
      {
        "field": "targetValue",
        "message": "Must be greater than baselineValue"
      }
    ]
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### 401 Unauthorized

```json
{
  "success": false,
  "error": {
    "code": "UNAUTHORIZED",
    "message": "Missing or invalid authentication token"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### 404 Not Found

```json
{
  "success": false,
  "error": {
    "code": "NOT_FOUND",
    "message": "Resource not found"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

### 500 Internal Server Error

```json
{
  "success": false,
  "error": {
    "code": "INTERNAL_ERROR",
    "message": "An internal server error occurred"
  },
  "timestamp": "2024-01-25T10:00:00Z"
}
```

## Rate Limiting

- **Rate Limit**: 1000 requests per hour per customer
- **Headers**:
  - `X-RateLimit-Limit`: Maximum requests allowed
  - `X-RateLimit-Remaining`: Requests remaining in current window
  - `X-RateLimit-Reset`: Unix timestamp when the limit resets

## Pagination

List endpoints support pagination with query parameters:

```bash
GET /customers/:id/value-definitions?page=1&pageSize=25&sort=created_at&order=desc
```

Response includes:

```json
{
  "items": [...],
  "total": 100,
  "page": 1,
  "pageSize": 25,
  "hasMore": true
}
```

## Webhooks

### Supported Events

- `customer.created`
- `setup.completed`
- `value.calculated`
- `approval.status_changed`
- `go_live.initiated`
- `ticket.created`
- `measurement.accuracy_updated`

### Webhook Payload

```json
{
  "event": "value.calculated",
  "timestamp": "2024-01-25T10:00:00Z",
  "data": {
    "customerId": "550e8400-e29b-41d4-a716-446655440000",
    "calculatedValue": 1250000,
    "previousValue": 1200000,
    "changePercentage": 4.17
  },
  "signature": "sha256=..."
}
```

---

**API Version**: 1.0
**Last Updated**: 2024-01-25
**Status**: Production Ready
