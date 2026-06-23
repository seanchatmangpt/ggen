#!/bin/bash
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/batch_rnc-compact-suite.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

# creates the test directory if it doesn't exist, and clears it, in case it already has contents
if [[ ${COMPACT_SUITE_HOME} ]]; then 
    echo "Starting Test of Compactifier XSLT"
    mkdir -p "${COMPACT_SUITE_HOME}"
    rm "${COMPACT_SUITE_HOME}"* >> /dev/null 2>&1; 
    rm "${COMPACT_SUITE_HOME}"*/* >> /dev/null 2>&1; 
    
    schemaname="lrml-compact.rnc"
    sfile="${DRIVER_HOME}${schemaname}"       
    "${BASH_HOME}aux_valrnc.sh" "${sfile}"
    if [[ "$?" -ne "0" ]]; then
       echo "Schema Validation Failed for ${schemaname}"
       exit 1
    fi   

    # Apply compactificaton XSLT transforamtions
    # transform files in RNC_TEST_SUITE_HOME ending in .lrml
    # output to COMPACT_SUITE_HOME (a temporary directory)
    # then validate (in separate for loop)
    for f in "${RNC_TEST_SUITE_HOME}"*/*.lrml
    do
      filename=$(basename "$f")
      "${BASH_HOME}aux_xslt.sh" "${f}" "${COMPACT_XSLT_HOME}lrml_compactifier.xslt" "${COMPACT_SUITE_HOME}${filename}"
      if [[ "$?" -ne "0" ]]; then
         echo "XSLT Transformation Failed"
         exit 1
       fi
    done

    for file in "${COMPACT_SUITE_HOME}"*.lrml
    do
        filename=$(basename "${file}")
        echo "File ${filename}"
        "${BASH_HOME}aux_valrnc.sh" "${sfile}" "${file}"
        exitvalue=$?
        if [[ ! "${file}" =~ fail ]] && [[ "${exitvalue}" -ne "0" ]]; then
            echo "Validation Failed for Compact ${file}"
            exit 1
        else
            if [[ "${file}" =~ fail ]] && [[ "${exitvalue}" == "0" ]]; then
               echo "Validation Succeeded for Compact Failure Test ${file}"
               exit 1
            fi
        fi       
    done
fi
