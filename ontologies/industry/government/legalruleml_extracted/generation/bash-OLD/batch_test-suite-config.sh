#!/bin/bash
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/batch_rnc-test-suite.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

# creates the output directories if they don't exist, and clears them of RNC files, in case they already have contents
testsrc1="${RNC_TEST_SUITE_HOME}compactified/"
#testsrc2="${RNC_TEST_SUITE_HOME}draft/"
#testsrc3="${RNC_TEST_SUITE_HOME}tutorial/"
testdir1="${RNC_TEST_SUITE_HOME}normalized/"
#testdir2="${RNC_TEST_SUITE_HOME}draft-normalized/"
testdir3="${RNC_TEST_SUITE_TMP}test_triples_no-ids/"
testdir4="${RNC_TEST_SUITE_TMP}test_triples_ids/"
testdir5="${COMPACT_SUITE_HOME}"
testdir6="${NORMAL_SUITE_HOME}"
testdir7="${RNC_TEST_SUITE_TMP}triplesMerger-ids/"
testdir8="${RNC_TEST_SUITE_TMP}normalized-canonicalized/"
#testdir9="${RNC_TEST_SUITE_TMP}draft-normalized-canonicalized/"
testdir10="${RNC_TEST_SUITE_TMP}triplesMerger-ids-noxi/"
mkdir -p "${testdir1}"
#mkdir -p "${testdir2}"
mkdir -p "${testdir3}"
mkdir -p "${testdir4}"
mkdir -p "${testdir5}"
mkdir -p "${testdir6}"
mkdir -p "${testdir7}"
mkdir -p "${testdir8}"
mkdir -p "${testdir9}"
mkdir -p "${testdir10}"
if [[ ${RNC_TEST_SUITE_TMP} ]]; then 
  rm "${testdir1}"*.lrml >> /dev/null 2>&1; 
  #rm "${testdir2}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir3}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir4}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir7}"*.rdf >> /dev/null 2>&1; 
  rm "${testdir8}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir9}"*.lrml >> /dev/null 2>&1; 
  rm "${testdir10}"*.rdf >> /dev/null 2>&1; 
fi
if [[ ${COMPACT_SUITE_HOME} ]]; then 
  rm "${COMPACT_SUITE_HOME}"*.lrml >> /dev/null 2>&1; 
fi
if [[ ${NORMAL_SUITE_HOME} ]]; then 
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

#for file in "${testsrc1}"*.lrml "${testsrc3}"*.lrml
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

for file in "${testdir8}"*.lrml "${testdir9}"*.lrml
do
  filename=$(basename "${file}")
    suf="-ids.lrml"
    newfilename="${filename/.lrml/$suf}"
    echo "File "${newfilename}
    cp "${file}" "${testdir3}${filename}"
    "${BASH_HOME}aux_xslt.sh" "${file}" "${COMPACT_XSLT_HOME}lrml_compactifier.xslt" "${testdir4}${newfilename}"
    "${BASH_HOME}aux_xslt.sh" "${testdir4}${newfilename}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-generate-id.xslt" "${testdir4}${newfilename}"
    "${BASH_HOME}aux_xslt.sh" "${testdir4}${newfilename}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${testdir4}${newfilename}"
done

for file in "${testdir3}"*.lrml "${testdir4}"*.lrml
do
  filename=$(basename "${file}")
  rdf="rdf"
  newfilename="${filename/lrml/$rdf}"
  echo "File "${newfilename}
  "${BASH_HOME}aux_xslt.sh" "${file}" "${SCHEMAS_HOMEXSLT_RDF_HOME}triplifyMerger-ids.xsl" "${testdir7}${newfilename}"
  "${BASH_HOME}aux_xslt.sh" "${file}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${testdir10}${newfilename}"

done


# Validate the generated RDF 
for file in "${testdir7}"*.rdf
do
  filename=$(basename "${file}")
  echo "File "${filename}
       schemaname="rdfxml.rnc"
       echo "Schema ${schemaname}"       
       sfile="${DRIVER_HOME}${schemaname}"       
       "${BASH_HOME}aux_valrnc.sh" "${sfile}" "${file}"
  if [[ "$?" -ne "0" ]]; then
           echo "Validation Failed for RDF ${file}"
           exit 1
       fi
done


