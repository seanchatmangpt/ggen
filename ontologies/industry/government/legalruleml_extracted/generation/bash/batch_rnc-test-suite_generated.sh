#!/bin/bash
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/batch_rnc-test-suite.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

if [[ ${RNC_TEST_SUITE_TMP} ]]; then 
  testdir3="${RNC_TEST_SUITE_TMP}test_triples_no-ids/"
  testdir4="${RNC_TEST_SUITE_TMP}test_triples_ids/"
  testdir7="${RNC_TEST_SUITE_TMP}triplesMerger-ids/"
  testdir8="${RNC_TEST_SUITE_TMP}normalized-canonicalized/"
  testdir10="${RNC_TEST_SUITE_TMP}triplesMerger-ids-noxi/"
fi
# FIXME - generate instances for testdir 8 and 9
# for file in "${testdir8}"*.lrml "${testdir9}"*.lrml
echo "Directory ${testdir8}"

for file in "${testdir8}"*.lrml
do
  filename=$(basename "${file}")
    echo "File ${file}"
    suf="-ids.lrml"
    newfilename="${filename/.lrml/$suf}"
    echo "File "${newfilename}
    cp "${file}" "${testdir3}${filename}"
    "${BASH_HOME}aux_xslt.sh" "${file}" "${COMPACT_XSLT_HOME}lrml_compactifier.xslt" "${testdir4}${newfilename}"
    "${BASH_HOME}aux_xslt.sh" "${testdir4}${newfilename}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-generate-id.xslt" "${testdir4}${newfilename}"
    "${BASH_HOME}aux_xslt.sh" "${testdir4}${newfilename}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${testdir4}${newfilename}"
done

#FIXME validate instances in testdir4?
