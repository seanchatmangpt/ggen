<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec" 
  xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->    
  <!--<xsl:import href="http://deliberation.ruleml.org/1.02/xslt/normalizer/1.02_normalizer_module.xslt"/>-->
  <xsl:import href="http://deliberation.ruleml.org/1.02/xslt/normalizer/1.02_normalizer_module.xslt"/>
  <xsl:import href="../normalizer/1.02_consumer_normalizer_module.xslt"/>
  <xsl:import href="../normalizer/lrml_normalizer_module.xslt"/>
  <xsl:import href="http://deliberation.ruleml.org/1.02/xslt/compactifier/1.02_deliberation_compactifier-ifthen_module.xslt"/>
  <xsl:import href="lrml_compactifier_module.xslt"/>
  <xsl:import href="http://deliberation.ruleml.org/1.02/xslt/normalizer/1.02_pretty-print_module.xslt"/>
  
  <!--Makes sure everything is printed nicely-->
  <xsl:variable name="pretty-print-output">
    <xsl:apply-templates select="$phase-compactify-output" mode="pretty-print">
      <xsl:with-param name="tabs">
        <xsl:text/>
      </xsl:with-param>
    </xsl:apply-templates>
  </xsl:variable>
  
    
</xsl:stylesheet>
