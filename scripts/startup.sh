#!/bin/bash
# Startup script for ggen project initialization
# Implements BIG BANG 80/20 screening gate before project can proceed
#
# Purpose: Prevent Seth-like patterns (custom ontologies, 3-month research, zero validation)
# by enforcing execution discipline: real data, standard ontologies, quick validation.

set -e

echo "🚀 ggen v26_5_19: BIG BANG 80/20 Screening Gate"
echo ""
echo "Before initializing, you must answer 5 questions about execution readiness."
echo "If you answer NO to any, stop and talk to Sean."
echo ""

# Screening Question 1: User Data
echo "❓ Question 1/5: Do you have real user data (CSV/JSON)?"
echo "   (Not promised. Actual files. If building a feature, do you have beta users' data?)"
echo "   Answer (yes/no):"
read -r q1
if [[ "$q1" != "yes" ]]; then
    echo "❌ STOP. You need real data to validate with. Build MVP first, use ggen after."
    exit 1
fi

# Screening Question 2: Standard Ontology
echo ""
echo "❓ Question 2/5: Can you find ONE existing standard ontology for your domain?"
echo "   (schema.org, FOAF, Dublin Core, SKOS - should take 5 min, not 3 months)"
echo "   Answer (yes/no):"
read -r q2
if [[ "$q2" != "yes" ]]; then
    echo "❌ STOP. You're about to build a custom ontology (Seth's mistake)."
    echo "   5 min: Find schema.org. 3 months: Build custom. Which path?"
    exit 1
fi

# Screening Question 3: Problem Articulation
echo ""
echo "❓ Question 3/5: Can you explain your problem in ONE sentence?"
echo "   (No 100-page documents. Just the core job-to-be-done.)"
echo "   Say it out loud, then answer (yes/no):"
read -r q3
if [[ "$q3" != "yes" ]]; then
    echo "❌ STOP. You don't have clarity. Write it down. One sentence. Try again."
    exit 1
fi

# Screening Question 4: Market Signal
echo ""
echo "❓ Question 4/5: Has anyone (not friends, not co-founders) committed to this?"
echo "   (Email list, signed beta contract, payment - PROOF, not enthusiasm)"
echo "   Answer (yes/no):"
read -r q4
if [[ "$q4" != "yes" ]]; then
    echo "⚠️  WARNING: Zero external validation. You're building in a vacuum."
    echo "   Proceed? (yes/no):"
    read -r q4_confirm
    if [[ "$q4_confirm" != "yes" ]]; then
        exit 1
    fi
fi

# Screening Question 5: Validation Speed
echo ""
echo "❓ Question 5/5: Can you validate with 10 real users in 48 hours?"
echo "   Answer (yes/no):"
read -r q5
if [[ "$q5" != "yes" ]]; then
    echo "⚠️  WARNING: You don't have a validation plan. How will you know if it works?"
    echo "   Proceed? (yes/no):"
    read -r q5_confirm
    if [[ "$q5_confirm" != "yes" ]]; then
        exit 1
    fi
fi

echo ""
echo "✅ Screening complete. You passed the litmus test."
echo ""

# Create directories if they don't exist
mkdir -p schema
mkdir -p templates
mkdir -p scripts
mkdir -p data

echo "📁 Project structure created:"
echo "   schema/           - Your ontology files (use standard bases, not custom)"
echo "   data/             - Your real user data (CSV/JSON)"
echo "   templates/        - Tera templates for code generation"
echo "   scripts/          - Custom scripts"

echo ""
echo "📋 Next steps (in order):"
echo "  1. Add your actual user data to data/ (CSV or JSON)"
echo "  2. Edit schema/domain.ttl with standard ontology (schema.org, FOAF, Dublin Core, SKOS)"
echo "  3. Create Tera templates in templates/ for your target language"
echo "  4. Run 'ggen sync --validate-only' to test without writing"
echo "  5. Run 'ggen sync' to generate code"
echo "  6. Validate with your 10 real users (not friends)"
echo ""
echo "⚡ Speed targets:"
echo "   - Data upload: 1 hour"
echo "   - Ontology selection: 1 hour (use standard, don't build custom)"
echo "   - Template creation: 2-4 hours"
echo "   - First user validation: 24 hours"
echo ""
echo "📚 Resources:"
echo "  - schema.org: https://schema.org/ (Google, Microsoft, Yahoo - trusted)"
echo "  - FOAF: http://xmlns.com/foaf/spec/ (Social networks)"
echo "  - Dublin Core: http://dublincore.org/ (Metadata)"
echo "  - SKOS: https://www.w3.org/2004/02/skos/ (Controlled vocabularies)"
echo "  - ggen Docs: https://docs.ggen.io"
echo "  - RDF/Turtle: https://www.w3.org/TR/turtle/"
echo "  - Tera: https://keats.github.io/tera/"
echo ""
echo "💡 Remember: Seth's problem was building a custom 100-page ontology instead of"
echo "   using schema.org in 5 minutes. Stay disciplined. Use standards first."
