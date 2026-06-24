#!/bin/bash
# XSD Build Script
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/build_xsd.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Fully local test script for XSD
# Dependencies
# batch_rnc2xsd.sh
# batch_xsd-test-suite.sh
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

# Generate XSD, and validate
"${BASH_HOME}batch_rnc2xsd.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Generation of XSD Failed"
     exit 1
fi
   
# Validate Examples in Test Suites
"${BASH_HOME}batch_xsd-test-suite.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of XSD Schema Failed"
     exit 1
fi

# Generate xml instances of lrml-normal and verify laws
"${BASH_HOME}generate_normal_xml.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of Laws on Normal-XSD-Conformant Instances Failed"
     exit 1
fi
# Generate xml instances of lrml-compact and verify laws
"${BASH_HOME}generate_compact_xml.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of Laws on COMPACT-XSD-Conformant Instances Failed"
     exit 1
fi
# Generate xml instances of lrml-basic and verify laws
"${BASH_HOME}generate_basic_xml.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of Laws on BASIC-XSD-Conformant Instances Failed"
     exit 1
fi

# Generate xml instances of lrml-normal, postprocess to RNC compliance and verify laws
"${BASH_HOME}generate_normal_xml_rnc.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of Laws on Normal-XSD+RNC-Conformant Instances Failed"
     exit 1
fi

# Generate xml instances of lrml-compact, postprocess to RNC compliance and verify laws
"${BASH_HOME}generate_compact_xml_rnc.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of Laws on COMPACT-XSD+RNC-Conformant Instances Failed"
     exit 1
fi

# Generate xml instances of lrml-basic, postprocess to RNC compliance and verify laws
"${BASH_HOME}generate_basic_xml_rnc.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of Laws on BASIC-XSD+RNC-Conformant Instances Failed"
     exit 1
fi
