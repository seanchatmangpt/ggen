<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec">
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->
  <!-- dc:description [ 'Transformation to enforce the additional constraint
       of sorted and unique indexing, while also reporting such changes via xsl:message.
       Appropriate for either compactified or normalized serializations at either XSD or RNC conformance level. ' ] -->
 
  <xsl:template match="*[ruleml:arg]">
    <xsl:variable name="args" select="./ruleml:arg"/>
    <xsl:for-each select="$args[position()>1]">
      <xsl:variable name="thisval" as="xs:integer"><xsl:value-of select="@index"/></xsl:variable>
      <xsl:variable name="previousval" as="xs:integer"><xsl:value-of select="./preceding-sibling::ruleml:arg[1]/@index"/></xsl:variable>
      <xsl:if test="not($thisval>$previousval)">
        <xsl:message>INVALID INDEX: should have been greater than <xsl:value-of select="$previousval"/>, was <xsl:value-of select="$thisval"/></xsl:message>
      </xsl:if>
    </xsl:for-each>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()[not(self::ruleml:arg or self::ruleml:repo or self::ruleml:slot or self::ruleml:resl)]"/>
      <xsl:apply-templates select="ruleml:arg">
        <xsl:sort select="@index"  data-type="number"/>                
      </xsl:apply-templates>
      <xsl:apply-templates select="ruleml:repo"/>
      <xsl:apply-templates select="ruleml:slot"/>
      <xsl:apply-templates select="ruleml:resl"/>
    </xsl:copy>  
  </xsl:template>

  <xsl:template match="*[ruleml:content]">
    <xsl:variable name="contents" select="./ruleml:content"/>
    <xsl:for-each select="$contents[position()>1]">
      <xsl:variable name="thisval" as="xs:integer"><xsl:value-of select="@index"/></xsl:variable>
      <xsl:variable name="previousval" as="xs:integer"><xsl:value-of select="./preceding-sibling::ruleml:content[1]/@index"/></xsl:variable>
      <xsl:if test="not($thisval>$previousval)">
        <xsl:message>INVALID INDEX: should have been greater than <xsl:value-of select="$previousval"/>, was <xsl:value-of select="$thisval"/></xsl:message>
      </xsl:if>
    </xsl:for-each>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()[not(self::ruleml:content or self::ruleml:repo or self::ruleml:slot or self::ruleml:resl)]"/>
      <xsl:apply-templates select="ruleml:content">
        <xsl:sort select="@index"  data-type="number"/>                
      </xsl:apply-templates>
      <xsl:apply-templates select="ruleml:repo"/>
      <xsl:apply-templates select="ruleml:slot"/>
      <xsl:apply-templates select="ruleml:resl"/>
    </xsl:copy>  
  </xsl:template>

  <xsl:template match="*[ruleml:formula[@index]]">
    <xsl:variable name="formulas" select="./ruleml:formula"/>
    <xsl:for-each select="$formulas[position()>1]">
      <xsl:variable name="thisval" as="xs:integer"><xsl:value-of select="@index"/></xsl:variable>
      <xsl:variable name="previousval" as="xs:integer"><xsl:value-of select="./preceding-sibling::ruleml:formula[1]/@index"/></xsl:variable>
      <xsl:if test="not($thisval>$previousval)">
        <xsl:message>INVALID INDEX: should have been greater than <xsl:value-of select="$previousval"/>, was <xsl:value-of select="$thisval"/></xsl:message>
      </xsl:if>
    </xsl:for-each>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()[not(self::ruleml:formula)]"/>
      <xsl:apply-templates select="ruleml:formula">
        <xsl:sort select="@index"  data-type="number"/>                
      </xsl:apply-templates>
    </xsl:copy>  
  </xsl:template>
  
  <!-- Copies everything else to the transformation output -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
