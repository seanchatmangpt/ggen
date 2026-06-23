#!/bin/bash
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# See ReadMe.text for instructions on running this script.

shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

mkdir -p "${INSTANCE_COMPACT_HOME}"
if [[ ${INSTANCE_COMPACT_HOME} ]]; then rm "${INSTANCE_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi

mkdir -p "${INSTANCE_SEQ_COMPACT_HOME}"
if [[ ${INSTANCE_SEQ_COMPACT_HOME} ]]; then rm "${INSTANCE_SEQ_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi

mkdir -p "${INSTANCE_SEQ_NORMAL_HOME}"
if [[ ${INSTANCE_SEQ_NORMAL_HOME} ]]; then rm "${INSTANCE_SEQ_NORMAL_HOME}"*.lrml  >> /dev/null 2>&1; fi

mkdir -p "${INSTANCE_LB_COMPACT_HOME}"
if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi

mkdir -p "${INSTANCE_LB_NORMAL_HOME}"
if [[ ${INSTANCE_LB_NORMAL_HOME} ]]; then rm "${INSTANCE_LB_NORMAL_HOME}"*.lrml  >> /dev/null 2>&1; fi

mkdir -p "${INSTANCE_RDF_HOME}"
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi

family="lrml-"
nschemanameNE="${family}normal"
cschemanameNE="${family}basic"
nschemaname="${nschemanameNE}.xsd"
cschemaname="${cschemanameNE}.xsd"
nxsfile="${XSD_NORMAL}${nschemaname}"       
cxsfile="${XSD_BASIC}${cschemaname}"       

# Validate XSD schema
  echo "Start XSD Schema Validation"
  "${BASH_HOME}aux_valxsd.sh" "${nxsfile}"
    if [[ "$?" -ne "0" ]]; then
     echo "Schema Validation Failed for ${nschemaname}"
       exit 1
   fi   
  "${BASH_HOME}aux_valxsd.sh" "${cxsfile}"
  if [[ "$?" -ne "0" ]]; then
       echo "Schema Validation Failed for ${cschemaname}"
       exit 1
   fi   

#   use oxygen to generate XML instances according to the configuration file for the basic driver
echo "Start Instance Generation"
"$GENERATE_SCRIPT" "$BASIC_CONFIG"

# Validate Both RNC schema
  echo "Start RNC Schema Validation"
  nschemaname="${nschemanameNE}.rnc"
  cschemaname="${cschemanameNE}.rnc"
  nsfile="${DRIVER_NORMAL_HOME}${nschemaname}"       
  csfile="${DRIVER_COMPACT_HOME}${cschemaname}"       
  "${BASH_HOME}aux_valrnc.sh" "${nsfile}"
  if [[ "$?" -ne "0" ]]; then
       echo "Schema Validation Failed for ${nschemaname}"
       exit 1
   fi   
  "${BASH_HOME}aux_valrnc.sh" "${csfile}"
  if [[ "$?" -ne "0" ]]; then
       echo "Schema Validation Failed for ${cschemaname}"
       exit 1
   fi   

# Apply XSLT transformations - instance postprocessing
# transform in place for files in INSTANCE_COMPACT_HOME
#
# Check number of files to start with
files=( "${INSTANCE_COMPACT_HOME}"*.lrml )
numfilesstart=${#files[@]}
echo "Number of Files to Start: ${numfilesstart}"

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Completing  ${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-compact-rnc.xslt" "${f}"
  if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation Failed"
     exit 1
   fi
done

# Validate instances
for file in "${INSTANCE_COMPACT_HOME}"*.lrml 
do
  filename=$(basename "${file}")
  echo "File ${filename}"
  "${BASH_HOME}aux_valxsd.sh" "${cxsfile}" "${file}"
  if [[ "$?" -ne "0" ]]; then
     echo "Completion Failed for  ${filename} - Removing"
     rm "${file}"
   else
    "${BASH_HOME}aux_valrnc.sh" "${csfile}" "${file}"
    if [[ "$?" -ne "0" ]]; then
       echo "RNC Validation Failed for  ${filename} - Removing"
       rm "${file}"
    fi
  fi       
done

# Check if too many files were removed
files=( "${INSTANCE_COMPACT_HOME}"*.lrml )
numfilesend=${#files[@]}
numfilesenddouble=2*$numfilesend
echo "Number of Files to End: ${numfilesend}"
  if [[ $numfilesenddouble -le $numfilesstart ]]; then
     echo "Completion Failed - Too Many Invalid Results"
     exit 1
   fi
   
# DONT Apply XSLT transforamtions to postprocess to ensure sequential indexing of edges with explicit index, like content
# BECAUSE this should already be satisfied due to the restrictions in the basic schema.
# transform INSTANCE_COMPACT_HOME into INSTANCE_SEQ_COMPACT_HOME
for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Enforce Sequential Indexing on ${filename}"
  fnew="${INSTANCE_SEQ_COMPACT_HOME}${filename}"
#  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-sequential-indexing.xslt" "${fnew}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
    if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation Failed for  ${filename}"
     exit 1
   fi
  "${BASH_HOME}aux_valxsd.sh" "${cxsfile}" "${fnew}"
  if [[ "$?" -ne "0" ]]; then
          echo "Validation Failed for ${fnew}"
          exit 1
   else
    "${BASH_HOME}aux_valrnc.sh" "${csfile}" "${fnew}"
    if [[ "$?" -ne "0" ]]; then
       echo "RNC Validation Failed for  ${fnew}"
       exit 1
    fi
  fi  
done 

# Apply XSLT transformations sequential indexing
# transform files in INSTANCE_SEQ_COMPACT_HOME
# Law: If w satisfies the basic schemas then Sw = w
#      where S is the transformation specified by xslt/validator/lrml_validator-sequential-indexing.xslt
#      that replaces each value of @index with its position among same-name siblings.
for f in "${INSTANCE_SEQ_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sequentializing  ${filename}"
  fnew="${INSTANCE_SEQ_COMPACT_HOME}re-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-sequential-indexing.xslt" "${fnew}"
  #"${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
  read -r firstlineold<"${f}"
  read -r firstlinenew<"${fnew}"
  echo "Sequentialized Comparing  ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Sequentialization Law Failed for  ${filename}"
     diff -q "${f}" "${fnew}" 
     exit 1
   fi
done

if [[ ${INSTANCE_SEQ_COMPACT_HOME} ]]; then rm "${INSTANCE_SEQ_COMPACT_HOME}"re-*.lrml  >> /dev/null 2>&1; fi

# Apply XSLT transformations for prefix evaluation
# transform files in INSTANCE_SEQ_COMPACT_HOME
# Law: If w satisfies the basic schemas then 
#            Vw = w
#      where V is the transformation specified by xslt/normalizer/lrml_prefix_evaluation.xslt
#      that replaces each CURIE, or CURIE-like abbreviation with its evaluation according to the prefix mapping.
for f in "${INSTANCE_SEQ_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sequentializing  ${filename}"
  fnew="${INSTANCE_SEQ_COMPACT_HOME}pre-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fnew}"
  #"${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
  read -r firstlineold<"${f}"
  read -r firstlinenew<"${fnew}"
  echo "Prefix-Evaluation Comparing  ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Prefix-Evaluation Law Failed for  ${filename}"
     exit 1
   fi
done

if [[ ${INSTANCE_SEQ_COMPACT_HOME} ]]; then rm "${INSTANCE_SEQ_COMPACT_HOME}"pre-*.lrml  >> /dev/null 2>&1; fi

# Apply XSLT transformations for compactifying
# transform files in INSTANCE_SEQ_COMPACT_HOME
# Law: If y satisfies the basic schemas 
#      then Cy = y
#      where C is the transformation specified by xslt/compactifier/lrml_compactifier.xslt
for f in "${INSTANCE_SEQ_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Re-Compactifying  ${filename}"
  fnew="${INSTANCE_SEQ_COMPACT_HOME}re-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${COMPACT_XSLT_HOME}lrml_compactifier.xslt" "${fnew}"
  "${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
  read -r firstlineold<"${f}"
  read -r firstlinenew<"${fnew}"
  echo "Re-Compactified Comparing  ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Re-Compactification Failed for  ${filename}"
     diff -q "${f}" "${fnew}" 
     exit 1
   fi
done

if [[ ${INSTANCE_SEQ_COMPACT_HOME} ]]; then rm "${INSTANCE_SEQ_COMPACT_HOME}"re-*.lrml  >> /dev/null 2>&1; fi

# Apply XSLT transforamtions - normalize, then compactify
# transform into new file with "rt-" prefix for files in INSTANCE_COMPACT_HOME
# Law: If y satisfies the basic XSD schema 
#      then CNy = y
for f in "${INSTANCE_SEQ_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Round-Trip Transforming  ${filename}"
  fnew="${INSTANCE_SEQ_COMPACT_HOME}rt-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fnew}"
  "${BASH_HOME}aux_xslt.sh" "${fnew}" "${COMPACT_XSLT_HOME}lrml_compactifier.xslt" "${fnew}"
  "${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
  read -r firstlineold<"${f}"
  read -r firstlinenew<"${fnew}"
  echo "Round-Trip Comparing  ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "XSLT Round Trip Failed for  ${filename}"
     diff -q "${f}" "${fnew}" 
     exit 1
   fi
done

if [[ ${INSTANCE_SEQ_COMPACT_HOME} ]]; then rm "${INSTANCE_SEQ_COMPACT_HOME}"rt-*.lrml  >> /dev/null 2>&1; fi


# Apply XSLT transforamtions for normalizing
# transform files in INSTANCE_COMPACT_HOME into INSTANCE_NORMAL_HOME 
for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Normalizing  ${filename}"
  fnew="${INSTANCE_NORMAL_HOME}${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fnew}"
  if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation Failed for  ${filename}"
     exit 1
   fi
  # Validate instances
  echo "Validating File ${filename}"
  "${BASH_HOME}aux_valxsd.sh" "${nxsfile}" "${fnew}"
  if [[ "$?" -ne "0" ]]; then
          echo "Validation Failed for ${file}"
          exit 1
   else
    "${BASH_HOME}aux_valrnc.sh" "${nsfile}" "${fnew}"
    if [[ "$?" -ne "0" ]]; then
       echo "RNC Validation Failed for ${filename}"
       exit 1
    fi
  fi       
done

if [[ ${INSTANCE_NORMAL_HOME} ]]; then rm "${INSTANCE_NORMAL_HOME}"*.lrml  >> /dev/null 2>&1; fi

# Apply XSLT normalization
# The result should satisfy the sequential indexing constraints.
# Law: If y satisfies the basic XSD schema 
#      then SNy = Ny
# Let R be the transformation that takes an instance that satisfies the normal XSD schemas and parses it to RDF.
for f in "${INSTANCE_SEQ_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Normalizing  ${filename}"
  fnew="${INSTANCE_SEQ_NORMAL_HOME}${filename}"
  fnews="${INSTANCE_SEQ_NORMAL_HOME}seq-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fnew}"
  "${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
  "${BASH_HOME}aux_xslt.sh" "${fnew}" "${VALIDATOR_XSLT_HOME}lrml_validator-sequential-indexing.xslt" "${fnews}"
  read -r firstlineold<"${fnew}"
  read -r firstlinenew<"${fnews}"
  echo "Checking Sequential Indexing of Normalized ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Sequential Indexing Validation Failed for  ${filename}"
     diff -q "${fnew}" "${fnews}" 
     exit 1
   fi
done

if [[ ${INSTANCE_SEQ_NORMAL_HOME} ]]; then rm "${INSTANCE_SEQ_NORMAL_HOME}"*.lrml  >> /dev/null 2>&1; fi

# Apply XSLT transforamtions to postprocess enforcing absence of attributes on leaf-branch edges, e.g. hasStrength
# transform INSTANCE_COMPACT_HOME into INSTANCE_LB_COMPACT_HOME
for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Enforce Sequential Indexing on ${filename}"
  fnew="${INSTANCE_LB_COMPACT_HOME}${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-leaf-branch.xslt" "${fnew}"
  #"${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
    if [[ "$?" -ne "0" ]]; then
     echo "XSLT Transformation Failed for  ${filename}"
     exit 1
   fi
  "${BASH_HOME}aux_valxsd.sh" "${cxsfile}" "${fnew}"
  if [[ "$?" -ne "0" ]]; then
          echo "Validation Failed for ${newfile}"
          exit 1
  fi       
done

# Re-apply XSLT leaf-branch transformations
# transform files in INSTANCE_LB_COMPACT_HOME
# Law: If y satisfies the basic schemas then BBy = By
#      where B is the transformation specified by xslt/validator/lrml_validator-leaf-branch.xslt
#      that removes attributes other than @xml:id or @index on branch edges.
for f in "${INSTANCE_LB_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Re-Sequentializing  ${filename}"
  fnew="${INSTANCE_LB_COMPACT_HOME}re-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-leaf-branch.xslt" "${fnew}"
  #"${BASH_HOME}aux_xslt.sh" "${fnew}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnew}"
  read -r firstlineold<"${f}"
  read -r firstlinenew<"${fnew}"
  echo "Re-Sequentialized Comparing  ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Re-Sequentialization Failed for  ${filename}"
     diff -q "${f}" "${fnew}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"re-*.lrml  >> /dev/null 2>&1; fi


# Apply XSLT normalization before leaf-branch constraint
# The result should be the same as applying normalization after leaf-branch constraint
# Law: If y satisfies the basic XSD schema, 
#      then BNy = NBy
for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Normalizing  ${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fn="${INSTANCE_NORMAL_HOME}${filename}"
  flbn="${INSTANCE_LB_NORMAL_HOME}lbn-${filename}"
  fnlb="${INSTANCE_LB_NORMAL_HOME}nlb-${filename}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fn}"
  "${BASH_HOME}aux_xslt.sh" "${fn}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fn}"
  "${BASH_HOME}aux_xslt.sh" "${fn}" "${VALIDATOR_XSLT_HOME}lrml_validator-leaf-branch.xslt" "${flbn}"
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-leaf-branch.xslt" "${flb}"
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fnlb}"
  "${BASH_HOME}aux_xslt.sh" "${fnlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fnlb}"
  read -r firstlineold<"${fnlb}"
  read -r firstlinenew<"${flbn}"
  echo "Checking Commutativity of Normalization and Leaf-Branch Constraint on ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Commutativity Failed for  ${filename}"
     diff -q "${flbn}" "${fnlb}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_NORMAL_HOME} ]]; then rm "${INSTANCE_LB_NORMAL_HOME}"*.lrml  >> /dev/null 2>&1; fi

# Let F = RVN where R is the "parsing" transformation "schemas/xslt/triplifyMerger-ids(-noxi).xsl
# F is the full parsing transformation for instances with compact conformance,
#  (validate against XSD and/or RNC compact schemas) 
# Need To Show: The following transformations are abstract-syntax preserving on basic-XSD conformant LegalRuleML instances
#   B, T, ST, C, N
#   while S alone is not abstract-syntax preserving. 
#   E.g. B is abstract-syntax preserving for some set of basic-XSD conformant instance documents A iff
#    FBv = Fv for all v in A

# B = transformation that enforces the leaf-branch edge constraint
# Need to show FBx = Fx for all XSD-basic conformant x

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Leaf-base Transforming  ${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fr="${INSTANCE_RDF_HOME}${filename}.rdf"
  frlb="${INSTANCE_RDF_HOME}lb-${filename}.rdf"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-leaf-branch.xslt" "${flb}"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fr}"
#  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fr}"
  
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${frlb}"
#  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${frlb}"
  
  read -r firstlineold<"${fr}"
  read -r firstlinenew<"${frlb}"
  echo "Checking Effect of Leaf-Branch Constraint on Parsing of ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Comparison Failed for  ${filename}"
     diff -q "${fr}" "${frlb}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi

# T = transformation that sorts edges with the index attribute by index value
# Need to show FTx = Fx for all XSD-basic conformant x

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sort Transforming  ${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fr="${INSTANCE_RDF_HOME}${filename}.rdf"
  frlb="${INSTANCE_RDF_HOME}lb-${filename}.rdf"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-sorted-indexing.xslt" "${flb}"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fr}"
#  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fr}"
  
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${frlb}"
#  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${frlb}"
  
  read -r firstlineold<"${fr}"
  read -r firstlinenew<"${frlb}"
  echo "Checking Effect of Sorting Indices on Parsing of ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Comparison Failed for  ${filename}"
     diff -q "${fr}" "${frlb}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi

# T = transformation that sorts edges with the index attribute by index value
# S = transformation that enforces sequential indexing without sorting first
# Need to show FSTx = Fx for all XSD-basic conformant x

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sort Transforming  ${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fr="${INSTANCE_RDF_HOME}${filename}.rdf"
  frlb="${INSTANCE_RDF_HOME}lb-${filename}.rdf"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${VALIDATOR_XSLT_HOME}lrml_validator-sorted-indexing.xslt" "${flb}"
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${VALIDATOR_XSLT_HOME}lrml_validator-sequential-indexing.xslt" "${flb}"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fr}"
#  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fr}"
  
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${frlb}"
#  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${frlb}"
  
  read -r firstlineold<"${fr}"
  read -r firstlinenew<"${frlb}"
  echo "Checking Effect of Sorting Indices and Then Enforcing Sequential Indexing on Parsing of ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Comparison Failed for  ${filename}"
     diff -q "${fr}" "${frlb}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi

# S = transformation that enforces sequential indexing without sorting first
# Need to show FSx != Fx for all XSD-basic conformant x where Tx != x

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sort Transforming  ${filename}"
  fold="${INSTANCE_LB_COMPACT_HOME}old-${filename}"
  fsort="${INSTANCE_LB_COMPACT_HOME}sort-${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fr="${INSTANCE_RDF_HOME}${filename}.rdf"
  frlb="${INSTANCE_RDF_HOME}lb-${filename}.rdf"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fold}"

  "${BASH_HOME}aux_xslt.sh" "${fold}" "${VALIDATOR_XSLT_HOME}lrml_validator-sorted-indexing.xslt" "${fsort}"

  read -r firstlineold<"${fold}"
  read -r firstlinenew<"${fsort}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then  
    "${BASH_HOME}aux_xslt.sh" "${fold}" "${VALIDATOR_XSLT_HOME}lrml_validator-sequential-indexing.xslt" "${flb}"
  
    "${BASH_HOME}aux_xslt.sh" "${fold}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fr}"
    "${BASH_HOME}aux_xslt.sh" "${fr}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fr}"
#  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${fr}"
    "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${fr}"
    "${BASH_HOME}aux_xslt.sh" "${fr}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fr}"
  
    "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${frlb}"
    "${BASH_HOME}aux_xslt.sh" "${frlb}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${frlb}"
#  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${frlb}"
    "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${frlb}"
    "${BASH_HOME}aux_xslt.sh" "${frlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${frlb}"

    read -r firstlineold<"${fr}"
    read -r firstlinenew<"${frlb}"
    echo "Checking Effect of Sequential Indexing Transform on Parsing of ${filename}"
    if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
      echo "Verified Sequential Indexing is not Abstract-Syntax Preserving for ${filename}"
    else
      echo "Failed Verification that Sequential Indexing is not Abstract-Syntax Preserving for ${filename}"
      #exit 1 
    fi
  fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi

# N = normalizer
# Need to show FNx = Fx for all XSD-basic conformant x

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sort Transforming  ${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fr="${INSTANCE_RDF_HOME}${filename}.rdf"
  frlb="${INSTANCE_RDF_HOME}lb-${filename}.rdf"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${flb}"
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${flb}"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fr}"
#  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fr}"
  
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${frlb}"
#  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${frlb}"
  
  read -r firstlineold<"${fr}"
  read -r firstlinenew<"${frlb}"
  echo "Checking Effect of Normalizer on Parsing of ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Comparison Failed for  ${filename}"
     diff -q "${fr}" "${frlb}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi

# C = compactifier
# Need to show FCx = Fx for all XSD-basic conformant x

for f in "${INSTANCE_COMPACT_HOME}"*.lrml
do
  filename=$(basename "$f")
  echo "Sort Transforming  ${filename}"
  flb="${INSTANCE_LB_COMPACT_HOME}${filename}"
  fr="${INSTANCE_RDF_HOME}${filename}.rdf"
  frlb="${INSTANCE_RDF_HOME}lb-${filename}.rdf"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${COMPACT_XSLT_HOME}lrml_compactifier.xslt" "${flb}"
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${flb}"
  
  "${BASH_HOME}aux_xslt.sh" "${f}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${fr}"
#  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${fr}"
  "${BASH_HOME}aux_xslt.sh" "${fr}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${fr}"
  
  "${BASH_HOME}aux_xslt.sh" "${flb}" "${NORMAL_XSLT_HOME}lrml_normalizer.xslt" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${NORMAL_XSLT_HOME}lrml_prefix_evaluation.xslt" "${frlb}"
#  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${XSLT_RDF_HOME}triplifyMerger-ids-noxi.xsl" "${frlb}"
  "${BASH_HOME}aux_xslt.sh" "${frlb}" "${INSTANCE_XSLT_HOME}lrml_instance-postprocessor-stripwhitespace.xslt" "${frlb}"
  
  read -r firstlineold<"${fr}"
  read -r firstlinenew<"${frlb}"
  echo "Checking Effect of Compactifier on Parsing of ${filename}"
  if [[ "${firstlineold}" != "${firstlinenew}" ]]; then
     echo "Comparison Failed for  ${filename}"
     diff -q "${fr}" "${frlb}" 
     exit 1
   fi
done

if [[ ${INSTANCE_LB_COMPACT_HOME} ]]; then rm "${INSTANCE_LB_COMPACT_HOME}"*.lrml  >> /dev/null 2>&1; fi
if [[ ${INSTANCE_RDF_HOME} ]]; then rm "${INSTANCE_RDF_HOME}"*.rdf  >> /dev/null 2>&1; fi
