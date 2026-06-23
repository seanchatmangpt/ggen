#!/bin/bash
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Auto-generate XSD from RNC
# Caution: Jing simplification cannot handle specified qualified names in content
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

# creates the temporary directory if they doesn't exist, and clears them, in case they already have contents
mkdir -p "${TMP_HOME}"
if [[ ${TMP_HOME} ]]; then rm "${TMP_HOME}"* >> /dev/null 2>&1; fi


# Finds the filename without extension
filename1=$(basename "$1")
echo "RNC Filename:  ${filename1}"
extension1="${filename1##*.}"
outdir=$(dirname "$2")

# creates the output directory if it doesn't exist, and clears it, in case it already has contents
mkdir -p "${outdir}"
if [[ ${outdir} ]]; then rm "${outdir}"* >> /dev/null 2>&1; fi

# Verifies that input schema name ends in ".rnc"
if [[ "${extension1}" != "rnc" ]];then
   echo "Input extension is not .rnc"
   exit 1
fi

# Finds the filename without extension
filename2=$(basename "$2")
echo "XSD Filename:  ${filename2}"
extension2="${filename2##*.}"

# Verifies that output name ends in ".xsd"
if [[ "${extension2}" != "xsd" ]];then
   echo "Output extension is not .xsd"
   exit 1
fi
infile="$1"
outfile="${TMP_HOME}${filename2}"
outfile2="${TMP_HOME}${5}.xsd"
echo "Input Filepath: ${infile}"
echo "Output Filepath: ${outfile}"
echo "Output Filepath: ${outfile2}"
if [[ "$3" == true ]]; then
    echo "Start simplification."
    java -jar "${JING}" -cs "$1" > "${TMP_RNG}"
    if [[ "$?" != "0" ]];then
      echo "Simplification Failed."
      exit 1
    fi
    infile="${TMP_RNG}"
    outfile="$2"
fi  

echo "Start conversion of  $infile"
java -jar "${TRANG}" -o disable-abstract-elements -o any-process-contents=lax "${infile}" "${outfile}"
if [[ "$?" != "0" ]];then
   echo "Conversion to XSD Failed."
   exit 1
fi
echo "Trang Conversion to XSD succeeded."

if [[ $3 != true ]]; then
  if [[ "${OXY_VERSION}" == 14 ]]; then
    echo "Start flattening of ${outfile} to ${outdir}"
    "${BASH_HOME}flatten_xsd.sh" "${outfile}" "${outdir}"
    echo "Start flattening of ${outfile2} to ${outdir}"
    "${BASH_HOME}flatten_xsd.sh" "${outfile2}" "${outdir}"
  else   
    echo "Start flattening of ${outfile} to $2"
    "${BASH_HOME}flatten_xsd.sh" "${outfile}" "$2"
    echo "Start flattening of ${outfile2} to ${outdir}"
    "${BASH_HOME}flatten_xsd.sh" "${outfile}" "${outdir}${6}.xsd"
  fi   
fi
if [[ $4 == true ]]; then
  function finish {
    if [[ ${TMP} ]]; then rm "${TMP}"; fi
  }
  trap finish EXIT
fi