#!/bin/bash
# MAIN Build Script
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/build.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Dependencies
# build_rnc.sh
# build_xsd.sh
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

now=$(date)
echo "Current date: $now"
# Test RNC
"${BASH_HOME}build_rnc.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Local Testing of RNC Schemas Failed"
     exit 1
fi
# Convert RNC to to XSD and test
"${BASH_HOME}build_xsd.sh"
if [[ "$?" -ne "0" ]]; then
     echo "Local Testing of XSD Schemas Failed"
     exit 1
fi

now=$(date)
echo "Current date: $now"
