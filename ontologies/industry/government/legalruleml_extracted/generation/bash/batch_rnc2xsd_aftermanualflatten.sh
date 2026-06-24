#!/bin/bash
# Auto-generate XSD from LegalRuleML RNC
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/bash/batch_rnc2xsd.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimhttp://consumer.ruleml.org/1.02/er: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

echo "Start final XSLT transformation of XSD from LegalRuleML RNC"

# Apply XSLT transforamtion
# transform in place for files in XSD_BASIC
for f in "${XSD_BASIC}"*.xsd
do
  filename=$(basename "$f")
  "${BASH_HOME}aux_xslt.sh" "${f}" "${XSLT2_HOME}basic-rnc2xsd.xslt" "${f}"
  if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation for BASIC XSD Failed"
     exit 1
   fi
done

# Apply XSLT transforamtion
# transform in place for files in XSD_COMPACT
for f in "${XSD_COMPACT}"*.xsd
do
  filename=$(basename "$f")
  "${BASH_HOME}aux_xslt.sh" "${f}" "${XSLT2_HOME}compact-rnc2xsd.xslt" "${f}"
  if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation for COMPACT XSD Failed"
     exit 1
   fi
done

# Apply XSLT transforamtion
# transform in place for files in XSD_NORMAL
for f in "${XSD_NORMAL}"*.xsd
do
  filename=$(basename "$f")
  "${BASH_HOME}aux_xslt.sh" "${f}" "${XSLT2_HOME}normal-rnc2xsd.xslt" "${f}"
  if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation for NORMAL XSD Failed"
     exit 1
   fi
done

# Validate the resulting XSD schemas
for f in "${XSD_BASIC}"*.xsd "${XSD_COMPACT}"*.xsd "${XSD_NORMAL}"*.xsd
do
  filename=$(basename "$f")
  echo "Validating  ${filename}"
  "${BASH_HOME}aux_valxsd.sh" "${f}"
  if [[ "$?" -ne "0" ]]; then
     echo "Validation Failed for  ${filename}"
     exit 1
   fi
done
