#!/bin/bash
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/batch_xsd-test-suite.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
#  Validate LegalRuleML instances by XSD
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

sfile="${XSD_COMPACT}lrml-compact.xsd"      
"${BASH_HOME}aux_valxsd.sh" "${sfile}"
  if [[ "$?" -ne "0" ]]; then
          echo "Schema Validation Failed for ${sfile}"
          exit 1
fi   
for file in "${COMPACT_SUITE_HOME}"*.lrml
do
  filename=$(basename "${file}")
  echo "File "${filename}
  "${BASH_HOME}aux_valxsd.sh" "${sfile}" "${file}"
  exitvalue=$?
  if [[ ! ${file} =~ fail ]] && [ "${exitvalue}" -ne "0" ]; then
          echo "Validation Failed for ${file}"
          exit 1
  else
         if [[ ${file} =~ fail ]] && [ "${exitvalue}" == "0" ]; then
           echo "Validation Succeeded for Failure Test ${file}"
           exit 1
         fi
  fi
done

sfile="${XSD_NORMAL}lrml-normal.xsd"      
"${BASH_HOME}aux_valxsd.sh" "${sfile}"
  if [[ "$?" -ne "0" ]]; then
          echo "Schema Validation Failed for ${sfile}"
          exit 1
fi   
for file in "${NORMAL_SUITE_HOME}"*.lrml
do
  filename=$(basename "${file}")
  echo "File "${filename}
  "${BASH_HOME}aux_valxsd.sh" "${sfile}" "${file}"
  exitvalue=$?
  if [[ ! ${file} =~ fail ]] && [ "${exitvalue}" -ne "0" ]; then
          echo "Validation Failed for ${file}"
          exit 1
  else
         if [[ ${file} =~ fail ]] && [ "${exitvalue}" == "0" ]; then
           echo "Validation Succeeded for Failure Test ${file}"
           exit 1
         fi
  fi
done
