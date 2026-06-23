#!/bin/bash
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/ ;. "${BASH_HOME}path_config.sh";

sfile="${SIMP_HOME}simp-lrml-compact.rnc"      
"${BASH_HOME}aux_valrnc.sh" "${sfile}"
  if [[ "$?" -ne "0" ]]; then
          echo "Schema Validation Failed for ${sfile}"
          exit 1
fi   
for file in "${TEST_SUITE_HOME}compact/"*.lrml
do
  filename=$(basename "${file}")
  echo "File ${filename}"
  "${BASH_HOME}aux_valrnc.sh" "${sfile}" "${file}"
  exitvalue=$?
  if [[ ! ${file} =~ fail ]] && [[ ! ${file} =~ data ]] && [ "${exitvalue}" -ne "0" ]; then
          echo "Validation Failed for ${file}"
          exit 1
  else
         if [[ ${file} =~ fail ]] && [ "${exitvalue}" == "0" ]; then
           echo "Validation Succeeded for Failure Test ${file}"
           exit 1
         fi
  fi
done

sfile="${SIMP_HOME}simp-lrml-normal.rnc"      
"${BASH_HOME}aux_valrnc.sh" "${sfile}"
  if [[ "$?" -ne "0" ]]; then
          echo "Schema Validation Failed for ${sfile}"
          exit 1
fi   
for file in "${TEST_SUITE_HOME}normal/"*.lrml
do
  filename=$(basename "${file}")
  echo "File ${filename}"
  "${BASH_HOME}aux_valrnc.sh" "${sfile}" "${file}"
  exitvalue=$?
  if [[ ! ${file} =~ fail ]] && [[ ! ${file} =~ data ]] &&[ "${exitvalue}" -ne "0" ]; then
          echo "Validation Failed for ${file}"
          exit 1
  else
         if [[ ${file} =~ fail ]] && [ "${exitvalue}" == "0" ]; then
           echo "Validation Succeeded for Failure Test ${file}"
           exit 1
         fi
  fi
done
