#!/bin/bash
# Auto-generate XSD from LegalRuleML RNC
# Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/bash/bash/batch_rnc2xsd.sh of Consumer RuleML 1.02
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimhttp://consumer.ruleml.org/1.02/er: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

echo "Start auto-generation of XSD from LegalRuleML RNC"
#
# creates the xsd directory if they doesn't exist, and clears them, in case they already have contents
mkdir -p "${XSD_HOME}"
mkdir -p "${XSD_BASIC}"
mkdir -p "${XSD_COMPACT}"
mkdir -p "${XSD_NORMAL}"
if [[ ${XSD_HOME} ]]; then rm "${XSD_HOME}"*.xsd >> /dev/null 2>&1; fi
if [[ ${XSD_BASIC} ]]; then rm "${XSD_BASIC}"*.xsd >> /dev/null 2>&1; fi
if [[ ${XSD_COMPACT} ]]; then rm "${XSD_COMPACT}"*.xsd >> /dev/null 2>&1; fi
if [[ ${XSD_NORMAL} ]]; then rm "${XSD_NORMAL}"*.xsd >> /dev/null 2>&1; fi

"${BASH_HOME}rnc2xsd_noflatten.sh" "${RNC4XSD_HOME}lrml4xsd-basic.rnc" "${XSD_BASIC}lrml-basic.xsd"
"${BASH_HOME}rnc2xsd_noflatten.sh" "${RNC4XSD_HOME}lrml4xsd-compact.rnc" "${XSD_COMPACT}lrml-compact.xsd"
"${BASH_HOME}rnc2xsd_noflatten.sh" "${RNC4XSD_HOME}lrml4xsd-normal.rnc" "${XSD_NORMAL}lrml-normal.xsd"
