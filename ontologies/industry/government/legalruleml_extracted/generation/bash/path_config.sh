#!/bin/bash
# dc:rights [ 'Copyright 2015 RuleML Inc. -- Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ]
# Configure path variables
# Dependencies:
# From oXygen XML
#   - Jing
#   - Trang
#   - Sax
#   - XSD flattening script
#   - schema docs generation script
# External libraries
#   - JAXB
# Installed packages
#  - java (1.6+)
#  - php5
#  - curl
#  - libxml2-utils (for xmllint)
#  - zip  
shopt -s nullglob
BASH_HOME=$( cd "$(dirname "$0")" ; pwd -P )/
PLATFORM="Linux"
OXY_VERSION=20
OXY_HOME="/mnt/c/Program Files/Oxygen XML Editor 20/"
FLATTEN_SCRIPT="${OXY_HOME}flattenSchema.sh"
GENERATE_SCRIPT="${OXY_HOME}xmlGenerator.sh"
DOC_SCRIPT="${OXY_HOME}schemaDocumentation.sh"
OXY_LIB="${OXY_HOME}lib/"
SAX_HOME="${OXY_LIB}"
JING="${OXY_LIB}jing.jar"
TRANG="${OXY_LIB}trang.jar"
JAXB_HOME="${OXY_LIB}"jaxb-ri-2.2.6/
REPO_HOME="${BASH_HOME}../../"
GEN_HOME="${REPO_HOME}generation/"
RNC_HOME="${REPO_HOME}relaxng/"
DRIVER_HOME="${REPO_HOME}relaxng/"
DRIVER_COMPACT_HOME="${REPO_HOME}relaxng/"
DRIVER_NORMAL_HOME="${REPO_HOME}relaxng/"
TMP_HOME="${REPO_HOME}../tmp/"
TMP="${TMP_HOME}tmp-std2xsd.rng"
TEST_SUITE_HOME="${GEN_HOME}test/"
DESIGN_HOME="${REPO_HOME}designPattern/"
#TEST_HOME="${RNC_HOME}test/"
RNC4SIMP_HOME="${GEN_HOME}drivers4simp/"
RNC4XSD_HOME="${GEN_HOME}" 
RNC_TEST_SUITE_HOME="${REPO_HOME}examples/"
RNC_TEST_SUITE_TMP="${TMP_HOME}rdf/"
TMP_MODULES="${TMP_HOME}modules/"
SIMP_HOME="${GEN_HOME}simplified/"
#XSD_HOME="${REPO_HOME}xsd/"
#XSD_MIN_HOME="${REPO_HOME}xsd_min/"
XSLT2_HOME="${GEN_HOME}xslt-xsd/"
XSD_HOME="${REPO_HOME}xsd-schema/"
XSD_BASIC="${XSD_HOME}basic/"
XSD_COMPACT="${XSD_HOME}compact/"
XSD_NORMAL="${XSD_HOME}normal/"
TEST_HOME="${TMP_HOME}generation/test/"
#XSD_TEST_SUITE_HOME="${TEST_HOME}rnc-test-suites/"
COMPACT_SUITE_HOME="${TEST_HOME}/compactifier-test-suites/"
NORMAL_SUITE_HOME="${TEST_HOME}/normalizer-test-suites/"
XSLT_HOME="${REPO_HOME}xslt/"
XSLT_XML_HOME="${XSLT_HOME}lrml-xml/"
XSLT_RDF_HOME="${XSLT_HOME}lrml-rdf/"
COMPACT_XSLT_HOME="${XSLT_XML_HOME}compactifier/"
NORMAL_XSLT_HOME="${XSLT_XML_HOME}normalizer/"
RDF_XSLT_HOME="${REPO_HOME}xslt/lrml-rdf/"
TMP_RNG="${TMP_HOME}tmp-std2xsd.rng"
MODULE_HOME="${RNC_HOME}modules/"
SUITE_HOME="${RNC_HOME}suites/"
TMPDIR="${SVN_HOME}/tmp/"
BASIC_CONFIG="${BASH_HOME}/settings/basic-config.xml"
COMPACT_CONFIG="${BASH_HOME}/settings/compact-config.xml"
NORMAL_CONFIG="${BASH_HOME}/settings/normal-config.xml"
INSTANCE_HOME="${TEST_HOME}/reaction-test-suites/"
INSTANCE_COMPACT_HOME="${TEST_HOME}/compact-test-suites/"
INSTANCE_NORMAL_HOME="${TEST_HOME}/normal-test-suites/"
INSTANCE_TEMP_HOME="${TEST_HOME}/tmp/"
INSTANCE_XSLT_HOME="${XSLT_XML_HOME}/instance-postprocessor/"
INSTANCE_SEQ_COMPACT_HOME="${TEST_HOME}/compact-seq-test-suites/"
INSTANCE_SEQ_NORMAL_HOME="${TEST_HOME}/normal-seq-test-suites/"
INSTANCE_LB_COMPACT_HOME="${TEST_HOME}/compact-lb-test-suites/"
INSTANCE_LB_NORMAL_HOME="${TEST_HOME}/normal-lb-test-suites/"
INSTANCE_RDF_HOME="${TEST_HOME}/rdf-test-suites/"
VALIDATOR_XSLT_HOME="${XSLT_XML_HOME}validator/"
