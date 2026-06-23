<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec">
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->
  <!-- dc:description [ 'Transformation to enforce the additional constraint
       of sequential indexing, while also reporting such changes via xsl:message.
       Appropriate for either compactified or normalized serializations at either XSD or RNC conformance level. ' ] -->
  
  <xsl:template match="ruleml:*[@index]">
    <xsl:variable name="edgename" select="name(.)"/>
    <xsl:variable name="indexval"><xsl:value-of select="@index"/></xsl:variable>
    <xsl:variable name="posval" select="count(./preceding-sibling::*[name()=$edgename])+1"/>
    <xsl:choose>
      <xsl:when test="$indexval = $posval">
        <xsl:copy>
          <xsl:apply-templates select="@*|node()"/>
        </xsl:copy>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message>INVALID INDEX: should have been <xsl:value-of select="$posval"/>, was <xsl:value-of select="$indexval"/></xsl:message>
        <xsl:copy>
          <xsl:apply-templates select="@*[not(name(.)='index')]"/>
          <xsl:attribute name="index" 
            select="$posval"/>
          <xsl:apply-templates select="node()"/>
        </xsl:copy>    
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <!-- Copies everything else to the transformation output -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
