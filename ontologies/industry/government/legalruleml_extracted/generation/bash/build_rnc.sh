#!/bin/bash
# RNC Build Script
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/build_rnc.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Fully local build script for RNC
# Dependencies
# indep_valid_modules/*.rnc
# aux_valrnc.sh
# batch_rnc2rng.sh
# batch_config2rnc.sh
# batch_config2rnc4simp.sh
# batch_rnc_test_suite.sh
# batch_rnc2simp.sh
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";
#
# Validate modules individually
echo "Start Module Validation"
for file in "${RNC_HOME}indep_valid_modules/"*.rnc
do
  "${BASH_HOME}aux_valrnc.sh" "${file}"
  if [[ "$?" -ne "0" ]]; then
     echo "Module Validation Failed"
     exit 1
  fi
done
# Validate drivers
echo "Start Driver Validation"
for file in "${DRIVER_HOME}"*.rnc
do
  "${BASH_HOME}aux_valrnc.sh" "${file}"
  if [[ "$?" -ne "0" ]]; then
     echo "Driver Validation Failed"
     exit 1
  fi
done
# Convert RNC to RNG, and validate against design
echo "Start Validation Against Schema Design"
"${BASH_HOME}batch_rnc2rng.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Validation Against Design Failed"
     exit 1
fi
# Simplify, and validate
echo "Start Generation of RNC Content Models"
"${BASH_HOME}batch_rnc2simp.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Simplification Failed"
     exit 1
fi
# Validate Examples in Relax NG Test Suites
echo "Start Validation of Examples in RNC Test Suites"
"${BASH_HOME}batch_rnc-test-suite.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of RNC Schemas Failed"
     exit 1
fi
# Compactify and Validate Examples from Relax NG Test Suites
"${BASH_HOME}batch_rnc-compact-suite.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of RNC Compact Schemas Failed"
     exit 1
fi
# Normalize and Validate Examples from Relax NG Test Suites
"${BASH_HOME}batch_rnc-normal-suite.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Testing of RNC Normalized Schemas Failed"
     exit 1
fi
echo "Testing Updated to This Point"

echo "Start Configuration of Test Examples"
"${BASH_HOME}batch_test-suite-config.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Configuration of Tests Failed"
     exit 1
fi

echo "Start Conversion from LRML to RDF"
"${BASH_HOME}batch_rnc-test-suite_rdf.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Conversion into RDF Failed"
     exit 1
fi
