<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec" 
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  >
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->    
  
  <!-- Functions -->
  <!-- test for skippable edges elements-->
  <xsl:function name="lrml:isSkippableEdge" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="
      namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and
      matches(local-name($node), '^has') and       
      not( matches(local-name($node), '^hasQualification$|^hasStrength$|hasTemplate$') )
      "
    />
  </xsl:function>
  
  <!-- Modifies the value of the xsi:schemaLocation attribute-->
  <xsl:template match="@xsi:schemaLocation" mode="phase-compactify">
    <xsl:attribute name="xsi:schemaLocation" select="replace(., 'normal', 'compact')"/>
  </xsl:template>
  
  <!-- Modifies the value of the xml-model processing instruction-->
  <xsl:template match="/processing-instruction('xml-model')" mode="phase-1">
    <xsl:variable name="text"><xsl:value-of select="."/></xsl:variable>
    <xsl:processing-instruction name="xml-model"><xsl:value-of select="replace($text, 'normal', 'compact' )"/></xsl:processing-instruction>
  </xsl:template>
  
  <!-- Remove skippable edges in the LegalRuleML namespace -->
  <xsl:template match="lrml:*[lrml:isSkippableEdge(.)][*]"  mode="phase-compactify">
    <xsl:apply-templates select="node()" mode="phase-compactify"/>        
  </xsl:template>  

  <!-- Also remove the skippable lrml:hasTemplate edges (not within lrml:FactualStatement) -->
  <xsl:template match="lrml:*[local-name(.)!='FactualStatement']/lrml:hasTemplate[*]"  mode="phase-compactify">
    <xsl:apply-templates select="node()" mode="phase-compactify"/>        
  </xsl:template>  
  
</xsl:stylesheet>
