#!/bin/bash
set -e

echo "🔍 Pack Query/Template Integration Verification"
echo "================================================"
echo ""

# Show test pack structure
echo "📦 Test Pack Structure:"
echo "----------------------"
ls -la ~/.ggen/packs/test-pack-integration/
echo ""

echo "📄 Pack Query (μ₂):"
echo "-------------------"
cat ~/.ggen/packs/test-pack-integration/queries/extract-test-entities.rq
echo ""

echo "📄 Pack Template (μ₃):"
echo "-----------------------"
cat ~/.ggen/packs/test-pack-integration/templates/test_entity.rs.tera
echo ""

echo "📄 Pack Ontology:"
echo "-----------------"
cat ~/.ggen/packs/test-pack-integration/ontology/pack.ttl
echo ""

echo "🔒 Lockfile Entry:"
echo "-----------------"
cat /Users/sac/ggen/.ggen/packs.lock | jq '.packs."test-pack-integration"'
echo ""

echo "✅ Integration Summary:"
echo "----------------------"
echo "1. Pack cache: ~/.ggen/packs/test-pack-integration/"
echo "2. Pack query: queries/extract-test-entities.rq (CONSTRUCT)"
echo "3. Pack template: templates/test_entity.rs.tera (Tera)"
echo "4. Lockfile: .ggen/packs.lock includes test-pack-integration"
echo "5. μ₀: PackResolver loads queries + templates from pack cache"
echo "6. μ₂: ExtractionPass.extend_with_pack_construct_queries()"
echo "7. μ₃: EmissionPass.extend_with_pack_templates()"
echo "8. μ₅: Receipt records pack provenance (queries + templates)"
echo ""

echo "🧪 To test: cargo test -p ggen-core --test pack_query_template_integration_test"
