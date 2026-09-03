#!/bin/bash
# Firestore Schema Initialization Script
# Creates collections, documents, and indexes for TAI Autonomics

set -e

PROJECT_ID=${1:-"tai-autonomics-prod"}
REGION=${2:-"us-central1"}

echo "🔧 Initializing Firestore schema for project: $PROJECT_ID"
echo "📍 Region: $REGION"
echo ""

# Set GCP project
gcloud config set project $PROJECT_ID

# ============================================================================
# 1. Create Collections and Indexes
# ============================================================================

echo "📚 Creating Firestore collections and indexes..."

# Collection: users
echo "Creating 'users' collection..."
gcloud firestore collections create users \
  --region=$REGION \
  2>/dev/null || echo "Collection 'users' already exists or being used"

# Collection: pricing_rules
echo "Creating 'pricing_rules' collection..."
gcloud firestore collections create pricing_rules \
  --region=$REGION \
  2>/dev/null || echo "Collection 'pricing_rules' already exists"

# Collection: subscriptions
echo "Creating 'subscriptions' collection..."
gcloud firestore collections create subscriptions \
  --region=$REGION \
  2>/dev/null || echo "Collection 'subscriptions' already exists"

# Collection: transactions
echo "Creating 'transactions' collection..."
gcloud firestore collections create transactions \
  --region=$REGION \
  2>/dev/null || echo "Collection 'transactions' already exists"

# Collection: audit_logs
echo "Creating 'audit_logs' collection..."
gcloud firestore collections create audit_logs \
  --region=$REGION \
  2>/dev/null || echo "Collection 'audit_logs' already exists"

# Collection: signals
echo "Creating 'signals' collection..."
gcloud firestore collections create signals \
  --region=$REGION \
  2>/dev/null || echo "Collection 'signals' already exists"

echo ""

# ============================================================================
# 2. Create Composite Indexes
# ============================================================================

echo "🔍 Creating composite indexes..."

# Index: subscriptions (user_id + status)
echo "Creating index: subscriptions (user_id + status)"
gcloud firestore indexes composite create \
  --collection=subscriptions \
  --field-config=user_id=Ascending,status=Ascending \
  2>/dev/null || echo "Index already exists or in progress"

# Index: transactions (user_id + created_at DESC)
echo "Creating index: transactions (user_id + created_at DESC)"
gcloud firestore indexes composite create \
  --collection=transactions \
  --field-config=user_id=Ascending,created_at=Descending \
  2>/dev/null || echo "Index already exists or in progress"

# Index: audit_logs (timestamp DESC)
echo "Creating index: audit_logs (timestamp DESC)"
gcloud firestore indexes composite create \
  --collection=audit_logs \
  --field-config=timestamp=Descending \
  2>/dev/null || echo "Index already exists or in progress"

# Index: signals (created_at DESC + status)
echo "Creating index: signals (created_at DESC + status)"
gcloud firestore indexes composite create \
  --collection=signals \
  --field-config=created_at=Descending,status=Ascending \
  2>/dev/null || echo "Index already exists or in progress"

echo ""

# ============================================================================
# 3. Create TTL Policies (optional)
# ============================================================================

echo "⏰ Configuring TTL policies..."

# TTL for signals (7 days)
echo "Setting TTL for 'signals' collection (7 days)..."
gcloud firestore fields patch signals.__default__ \
  --ttl-days=7 \
  2>/dev/null || echo "TTL configuration may require UI access"

echo ""

# ============================================================================
# 4. Create Sample Data (for testing)
# ============================================================================

echo "📝 Creating sample data..."

# Create sample user
echo "Creating sample user..."
gcloud firestore documents create users/user-demo-001 \
  --data="name=Demo User,email=demo@example.com,created_at=$(date -u +%Y-%m-%dT%H:%M:%SZ),status=active" \
  2>/dev/null || echo "Sample user may already exist"

# Create sample pricing rule
echo "Creating sample pricing rule..."
gcloud firestore documents create pricing_rules/rule-standard-monthly \
  --data="name=Standard Monthly,price=99.99,currency=USD,billing_cycle=monthly,status=active" \
  2>/dev/null || echo "Sample pricing rule may already exist"

echo ""

# ============================================================================
# 5. Verify Schema Creation
# ============================================================================

echo "✅ Verifying Firestore schema..."

echo ""
echo "Collections in Firestore:"
gcloud firestore collections list --limit=10

echo ""
echo "Indexes in progress or created:"
gcloud firestore indexes list --limit=10 2>/dev/null || echo "No custom indexes or checking via Console"

echo ""
echo "Sample documents:"
echo "Users: $(gcloud firestore documents list --collection=users 2>/dev/null | wc -l)"
echo "Pricing Rules: $(gcloud firestore documents list --collection=pricing_rules 2>/dev/null | wc -l)"

echo ""
echo "✨ Firestore schema initialization complete!"
echo ""
echo "📖 Documentation:"
echo "   Collections: users, pricing_rules, subscriptions, transactions, audit_logs, signals"
echo "   Indexes: Check Cloud Console → Firestore → Indexes for status"
echo "   Sample Data: Check Cloud Console → Firestore → Data for created documents"
echo ""
echo "🚀 Next steps:"
echo "   1. Verify indexes are built (may take a few minutes)"
echo "   2. Deploy Cloud Run services"
echo "   3. Test connectivity from application"
echo ""
