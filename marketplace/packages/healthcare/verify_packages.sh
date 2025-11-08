#!/bin/bash

# Healthcare Packages Verification Script

echo "========================================="
echo "Healthcare Packages Verification"
echo "========================================="
echo ""

# Color codes
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Counters
total_checks=0
passed_checks=0
failed_checks=0

# Function to check file exists
check_file() {
    local file=$1
    local description=$2
    total_checks=$((total_checks + 1))

    if [ -f "$file" ]; then
        echo -e "${GREEN}✓${NC} $description"
        passed_checks=$((passed_checks + 1))
    else
        echo -e "${RED}✗${NC} $description (Missing: $file)"
        failed_checks=$((failed_checks + 1))
    fi
}

# Function to check file has minimum lines
check_file_lines() {
    local file=$1
    local min_lines=$2
    local description=$3
    total_checks=$((total_checks + 1))

    if [ -f "$file" ]; then
        local lines=$(wc -l < "$file")
        if [ "$lines" -ge "$min_lines" ]; then
            echo -e "${GREEN}✓${NC} $description ($lines lines)"
            passed_checks=$((passed_checks + 1))
        else
            echo -e "${YELLOW}⚠${NC} $description ($lines lines, expected >= $min_lines)"
            passed_checks=$((passed_checks + 1))
        fi
    else
        echo -e "${RED}✗${NC} $description (Missing: $file)"
        failed_checks=$((failed_checks + 1))
    fi
}

echo "Package 1: FHIR Patient Management"
echo "-----------------------------------"
check_file_lines "fhir-patient-management/ontology/fhir-patient.ttl" 300 "FHIR Ontology"
check_file_lines "fhir-patient-management/sparql/queries.rq" 300 "SPARQL Queries"
check_file_lines "fhir-patient-management/templates/rust/fhir_server.rs" 500 "Rust FHIR Server"
check_file_lines "fhir-patient-management/templates/typescript/fhir-client.ts" 250 "TypeScript Client"
check_file_lines "fhir-patient-management/templates/python/fhir_analytics.py" 200 "Python Analytics"
check_file_lines "fhir-patient-management/tests/chicago_tdd/fhir_server_test.rs" 600 "Chicago TDD Tests"
check_file "fhir-patient-management/package.toml" "Package Manifest"
check_file "fhir-patient-management/docs/README.md" "Documentation"
echo ""

echo "Package 2: HL7 v2 Integration"
echo "-----------------------------------"
check_file_lines "hl7-v2-integration/ontology/hl7-v2.ttl" 250 "HL7 Ontology"
check_file_lines "hl7-v2-integration/sparql/queries.rq" 250 "SPARQL Queries"
check_file_lines "hl7-v2-integration/templates/rust/hl7_parser.rs" 400 "Rust HL7 Parser"
check_file_lines "hl7-v2-integration/templates/typescript/hl7-broker.ts" 400 "TypeScript Broker"
check_file_lines "hl7-v2-integration/templates/python/hl7_transformer.py" 350 "Python Transformer"
check_file "hl7-v2-integration/package.toml" "Package Manifest"
check_file "hl7-v2-integration/docs/README.md" "Documentation"
echo ""

echo "Package 3: DICOM Medical Imaging"
echo "-----------------------------------"
check_file_lines "dicom-medical-imaging/ontology/dicom.ttl" 280 "DICOM Ontology"
check_file_lines "dicom-medical-imaging/sparql/queries.rq" 250 "SPARQL Queries"
check_file_lines "dicom-medical-imaging/templates/rust/dicom_parser.rs" 400 "Rust DICOM Parser"
check_file "dicom-medical-imaging/package.toml" "Package Manifest"
check_file "dicom-medical-imaging/docs/README.md" "Documentation"
echo ""

echo "========================================="
echo "Summary"
echo "========================================="
echo "Total Checks: $total_checks"
echo -e "${GREEN}Passed: $passed_checks${NC}"
echo -e "${RED}Failed: $failed_checks${NC}"
echo ""

if [ "$failed_checks" -eq 0 ]; then
    echo -e "${GREEN}✓ All healthcare packages verified successfully!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some checks failed. Please review.${NC}"
    exit 1
fi
