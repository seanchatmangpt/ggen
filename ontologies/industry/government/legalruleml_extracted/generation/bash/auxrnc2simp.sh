#!/bin/bash
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Auto-generate Simplified RNC from any RNC
#   - makes modular RNC monolothic
#   - removes intermediate named patterns
#   - simplified definitions
# Prerequisites:
#   Installation of Java and Jing/Trang. See https://code.google.com/p/jing-trang/
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";
# simplify to RNG
java -jar "${JING}" -cs $1 > "${RNC_HOME}tmp/tmp-std2xsd.rng"
# convert to value of second argument
java -jar "${TRANG}" "${RNC_HOME}tmp/tmp-std2xsd.rng" $2
#rm ${RELAXNG_HOME}tmp-std2xsd.rng