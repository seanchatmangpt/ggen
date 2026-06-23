#!/bin/bash
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/batch_rnc-test-suite.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";
if [[ ${RNC_TEST_SUITE_TMP} ]]; then 
  testdir3="${RNC_TEST_SUITE_TMP}test_triples_no-ids/"
  testdir4="${RNC_TEST_SUITE_TMP}test_triples_ids/"
  testdir7="${RNC_TEST_SUITE_TMP}triplesMerger-ids/"
  testdir10="${RNC_TEST_SUITE_TMP}triplesMerger-ids-noxi/"
  mkdir -p "${testdir7}"
  mkdir -p "${testdir10}"
  rm "${testdir7}"*.rdf >> /dev/null 2>&1; 
  rm "${testdir10}"*.rdf >> /dev/null 2>&1; 

fi
#for file in "${testdir3}"*.lrml "${testdir4}"*.lrml

for file in "${testdir3}"*.lrml
do
  filename=$(basename "${file}")
  rdf="rdf"
  newfilename="${filename/lrml/$rdf}"
  echo "File "${newfilename}
  "${BASH_HOME}aux_xslt.sh" "${file}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${testdir7}${newfilename}"
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

#FIXME What about testdir10?