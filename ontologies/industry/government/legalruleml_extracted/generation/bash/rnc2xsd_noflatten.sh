#!/bin/bash
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Auto-generate unflattened XSD from RNC
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

# Finds the input filename and its extension
filename1=$(basename "$1")
extension1="${filename1##*.}"

# Verifies that input schema name ends in ".rnc"
if [[ "${extension1}" != "rnc" ]];then
   echo "Input extension is not .rnc"
   exit 1
fi

# Finds the output filename and its extension
filename2=$(basename "$2")
extension2="${filename2##*.}"

# Find the output directory and create it if necessary
outdir=$(dirname "$2")
mkdir -p "${outdir}"
rm "${outdir}"/*.xsd >> /dev/null 2>&1

# Verifies that output name ends in ".xsd"
if [[ "${extension2}" != "xsd" ]];then
   echo "Output extension is not .xsd"
   exit 1
fi

infile="$1"
outfile="$2"

echo "Start conversion of ${infile} to ${outfile}"
java -jar "${TRANG}" -o disable-abstract-elements -o any-process-contents=lax "${infile}" "${outfile}"
if [[ "$?" != "0" ]];then
   echo "Conversion to XSD Failed."
   exit 1
fi
echo "Trang Conversion to XSD succeeded."
