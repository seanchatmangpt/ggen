#!/bin/bash
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

instance="$1"
len=$((53+${#instance}))

serialization="normal"
conformance="normalized"
XSD_DIR="${XSD_HOME}${serialization}/"

# check conformance with normal RNC schema
  RNCSchema="${DRIVER_HOME}lrml-${serialization}.rnc"       
  "${BASH_HOME}aux_valrnc.sh" "${RNCSchema}" "${instance}"
  exitval="$?"
  echo; eval printf '*%.0s' {1..$len}; echo
  if [[ "$exitval" -ne "0" ]]; then
       echo "* ${instance} is not RNC-${conformance} conformant"
   else    
       echo "* ${instance} is RNC-${conformance} conformant"
   fi   
  eval printf '*%.0s' {1..$len}; echo $'\n'
# check conformance with XSD schema
  XSDSchema="${XSD_DIR}lrml-${serialization}.xsd"       
  "${BASH_HOME}aux_valxsd.sh" "${XSDSchema}" "${instance}"
  exitval="$?"
  echo; eval printf '*%.0s' {1..$len}; echo
  if [[ "$exitval" -ne "0" ]]; then
       echo "* ${instance} is not XSD-${conformance} conformant"
   else    
       echo "* ${instance} is XSD-${conformance} conformant"
   fi   
  eval printf '*%.0s' {1..$len}; echo $'\n'
# check conformance with Prefix-Evaluation Constraint
  mkdir -p "${TMP_HOME}"
  instanceFilename=$(basename "${instance}")
  prefixEvalInstance="${TMP_HOME}pe-${instanceFilename}"

  "${BASH_HOME}aux_xslt.sh" "${instance}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${prefixEvalInstance}"
  "${BASH_HOME}aux_valrnc.sh" "${RNCSchema}" "${prefixEvalInstance}"
  exitvalrnc="$?"
  "${BASH_HOME}aux_valxsd.sh" "${XSDSchema}" "${prefixEvalInstance}"
  exitvalxsd="$?"
  echo; eval printf '*%.0s' {1..$len}; echo
  if [[ "$exitvalrnc" -ne "0" ]]; then
       echo "* ${instance} is not Prefix-Evaluation-RNC-${conformance} conformant"
   else    
       echo "* ${instance} is Prefix-Evaluation-RNC-${conformance} conformant"
   fi   
  eval printf '*%.0s' {1..$len}; echo
  if [[ "$exitvalxsd" -ne "0" ]]; then
       echo "* ${instance} is not Prefix-Evaluation-XSD-${conformance} conformant"
   else    
       echo "* ${instance} is Prefix-Evaluation-XSD-${conformance} conformant"
   fi   
  eval printf '*%.0s' {1..$len}; echo $'\n'
# check conformance with Sequential-Index Constraint (only)
  seqIndexInstance="${TMP_HOME}seq-${instanceFilename}"
  echo "Checking ${instance} for Sequential-Index conformance ..."
  "${BASH_HOME}aux_xslt.sh" "${instance}" "${VALIDATOR_XSLT_HOME}lrml_validator-sequential-indexing.xslt" "${seqIndexInstance}"
  echo; eval printf '*%.0s' {1..$len}; echo
  echo "* If no INVALID INDEX messages were logged, then "
  echo "*   ${instance} is Sequential-Index conformant"
  eval printf '*%.0s' {1..$len}; echo $'\n'
# check conformance with Leaf-Branch Constraint (only)
  leafBranchInstance="${TMP_HOME}lb-${instanceFilename}"
  echo "Checking ${instance} for Leaf-Branch conformance ..."
  "${BASH_HOME}aux_xslt.sh" "${instance}" "${VALIDATOR_XSLT_HOME}lrml_validator-leaf-branch.xslt" "${leafBranchInstance}"
  echo; eval printf '*%.0s' {1..$len}; echo
  echo "* If no INVALID INDEX messages were logged, then "
  echo "*   ${instance} is Leaf-Branch conformant"
  eval printf '*%.0s' {1..$len}; echo $'\n'
