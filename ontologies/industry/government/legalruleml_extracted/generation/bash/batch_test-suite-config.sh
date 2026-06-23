#!/bin/bash
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/batch_rnc-test-suite.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

if [[ ${RNC_TEST_SUITE_HOME} ]]; then 
  testsrc1="${RNC_TEST_SUITE_HOME}compactified/"
  testsrc2="${RNC_TEST_SUITE_HOME}draft/"
  testdir1="${RNC_TEST_SUITE_HOME}normalized/"
  testdir2="${RNC_TEST_SUITE_HOME}draft-normalized/"
  mkdir -p "${testdir1}"
  mkdir -p "${testdir2}"
  rm "${testdir1}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir2}"*.lrml >> /dev/null 2>&1; 
fi
if [[ ${RNC_TEST_SUITE_TMP} ]]; then 
  testdir3="${RNC_TEST_SUITE_TMP}test_triples_no-ids/"
  testdir4="${RNC_TEST_SUITE_TMP}test_triples_ids/"
  testdir7="${RNC_TEST_SUITE_TMP}triplesMerger-ids/"
  testdir8="${RNC_TEST_SUITE_TMP}normalized-canonicalized/"
  testdir10="${RNC_TEST_SUITE_TMP}triplesMerger-ids-noxi/"
  mkdir -p "${testdir3}"
  mkdir -p "${testdir4}"
  mkdir -p "${testdir7}"
  mkdir -p "${testdir8}"
  mkdir -p "${testdir10}"
  rm "${testdir3}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir4}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir7}"*.rdf >> /dev/null 2>&1; 
  rm "${testdir8}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir10}"*.rdf >> /dev/null 2>&1; 
fi

if [[ ${COMPACT_SUITE_HOME} ]]; then 
  testdir5="${COMPACT_SUITE_HOME}"
  mkdir -p "${testdir5}"
  rm "${COMPACT_SUITE_HOME}"*.lrml >> /dev/null 2>&1; 
fi
if [[ ${NORMAL_SUITE_HOME} ]]; then 
  testdir6="${NORMAL_SUITE_HOME}"
  mkdir -p "${testdir6}"
  rm "${NORMAL_SUITE_HOME}"*.lrml >> /dev/null 2>&1; 
fi

  schemaname="lrml-normal.rnc"
  snfile="${DRIVER_HOME}${schemaname}"       
  "${BASH_HOME}aux_valrnc.sh" "${snfile}"
  if [[ "$?" -ne "0" ]]; then
       echo "Schema Validation Failed for ${schemaname}"
       exit 1
   fi   

  schemaname="lrml-compact.rnc"
  scfile="${DRIVER_HOME}${schemaname}"       
  "${BASH_HOME}aux_valrnc.sh" "${scfile}"
  if [[ "$?" -ne "0" ]]; then
       echo "Schema Validation Failed for ${schemaname}"
       exit 1
   fi   

# Starting with manual-written instances in the compact serialization
# Apply the normalizer, output going to testdir1
# Validate against the schema for the normalized serialization
for file in "${testsrc1}"*.lrml
do
  filename=$(basename "${file}")
  sub="normal"
  newfilename="${filename/compact/$sub}"
  echo "File "${newfilename}
  "${BASH_HOME}aux_xslt.sh" "${file}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${testdir1}${newfilename}"
  "${BASH_HOME}aux_valrnc.sh" "${snfile}" "${testdir1}${newfilename}"
    exitvalue=$?
    if [[ ! "${file}" =~ fail ]] && [[ "${exitvalue}" -ne "0" ]]; then
          echo "Validation 0 Failed for Normal ${newfilename}"
          exit 1
     else
        if [[ "${file}" =~ fail ]] && [[ "${exitvalue}" == "0" ]]; then
           echo "Validation 0 Succeeded for Normal Failure Test ${newfilename}"
           exit 1
         fi
    fi       
    
# Apples the XSLT for prefix evaluation (of instances in normalized serialization)
# Output goes to testdir8
# Validates the output    
  sub="normal-prefix-evaluated"
  newfilenameca="${filename/compact/$sub}"
  "${BASH_HOME}aux_xslt.sh" "${testdir1}${newfilename}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${testdir8}${newfilenameca}"
  "${BASH_HOME}aux_valrnc.sh" "${snfile}" "${testdir8}${newfilenameca}"
    exitvalue=$?
    if [[ ! "${file}" =~ fail ]] && [[ "${exitvalue}" -ne "0" ]]; then
          echo "Validation 1 Failed for Normal ${newfilenameca}"
          exit 1
     else
        if [[ "${file}" =~ fail ]] && [[ "${exitvalue}" == "0" ]]; then
           echo "Validation 1 Succeeded for Normal Failure Test ${newfilenameca}"
           exit 1
         fi
    fi       
done
exit 0
# This section is used when examples are under development
#for file in "${testsrc2}"*.lrml
#do
#  filename=$(basename "${file}")
#  normal="normal"
#  newfilename="${filename/compact/$normal}"
#  echo "File "${newfilename}
#  "${BASH_HOME}aux_xslt.sh" "${file}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${testdir2}${newfilename}"
#  "${BASH_HOME}aux_valrnc.sh" "${snfile}" "${testdir2}${newfilename}"
#    exitvalue=$?
#    if [[ ! "${file}" =~ fail ]] && [[ "${exitvalue}" -ne "0" ]]; then
#          echo "Validation 2 Failed for Normal ${file}"
#          exit 1
#     else
#        if [[ "${file}" =~ fail ]] && [[ "${exitvalue}" == "0" ]]; then
#           echo "Validation 2 Succeeded for Normal Failure Test ${file}"
#           exit 1
#         fi
#    fi
#  sub="normal-prefix-evaluated"
#  newfilenameca="${filename/compact/$sub}"
#  "${BASH_HOME}aux_xslt.sh" "${testdir2}${newfilename}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${testdir9}${newfilenameca}"
#  "${BASH_HOME}aux_valrnc.sh" "${snfile}" "${testdir9}${newfilenameca}"
#    exitvalue=$?
#    if [[ ! "${file}" =~ fail ]] && [[ "${exitvalue}" -ne "0" ]]; then
#          echo "Validation 3 Failed for Normal ${newfilenameca}"
#          exit 1
#     else
#        if [[ "${file}" =~ fail ]] && [[ "${exitvalue}" == "0" ]]; then
#           echo "Validation 3 Succeeded for Normal Failure Test ${newfilenameca}"
#           exit 1
#         fi
#    fi       
#done








