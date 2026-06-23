<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/" xmlns:ruleml="http://ruleml.org/spec"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema"
  exclude-result-prefixes="xsi xs">
  <!-- Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/xslt/normalizer/1.02_normalizer.xslt of Consumer RuleML 1.02 -->
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->
  <!--<xsl:import href="http://deliberation.ruleml.org/1.02/xslt/normalizer/1.02_normalizer_module.xslt"/>
  <xsl:import href="1.02_consumer_normalizer_module.xslt"/>
  <xsl:import href="lrml_normalizer_module.xslt"/>
  <xsl:import href="../compactifier/lrml_compactifier_module.xslt"/>-->
  <xsl:strip-space elements="*"/>

  <xsl:variable name="debug" as="xs:boolean" select="false()"/>

  <xsl:function name="lrml:canBePrefixed" as="xs:boolean">
    <xsl:param name="this" as="attribute()"/>
    <xsl:value-of
      select="matches(local-name($this), 
      '^memberType$|^refType$|^refID$|^refIDSystemSource$|^hasCreationDate$|^sameAs$|^iri$|^type$|^over$|^under$|^closure$|^style$|^iri$'
      )"
    />
  </xsl:function>

  <xsl:function name="lrml:isPrefixFor" as="xs:boolean">
    <xsl:param name="attpre" as="attribute()"/>
    <xsl:param name="value" as="xs:string"/>
    <xsl:variable name="pre"><xsl:value-of select="$attpre"/></xsl:variable>
    <xsl:variable name="pref" select="concat($pre,':')"/>
    <xsl:variable name="rpref" select="concat('^', $pref)"/>
    <xsl:value-of select="matches($value, $rpref)"/>
  </xsl:function>


  <xsl:function name="lrml:resolve">
    <xsl:param name="attvalue" as="xs:string"/>
    <xsl:variable name="prefixmatch" select="$prefixes[lrml:isPrefixFor(./@pre, $attvalue)]"/>
    <xsl:choose>
      <xsl:when test="count($prefixmatch) > 0">
        <xsl:variable name="pref"><xsl:value-of select="$prefixmatch[1]/@pre"/>:</xsl:variable>
        <xsl:variable name="refID">
          <xsl:value-of select="$prefixmatch[1]/@refID"/>
        </xsl:variable>
        <xsl:value-of select="replace($attvalue, $pref, $refID )"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:copy-of select="$attvalue"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <!-- Get the Prefixes -->
  <xsl:variable name="prefixes" select="/*/lrml:hasPrefix/*"/>

  <!-- Resolve CURIEs and CURIE-like prefix abbreviations -->
  <xsl:template match="@*[lrml:canBePrefixed(.)]">
    <xsl:if test="$debug">
      <xsl:message>MATCHED <xsl:value-of select="name()"/></xsl:message>
    </xsl:if>
    <xsl:variable name="attvalue">
      <xsl:value-of select="."/>
    </xsl:variable>
    <xsl:if test="$debug"><xsl:message>ORIGINAL <xsl:value-of select="$attvalue"/></xsl:message></xsl:if>
    <xsl:attribute name="{name()}" select="lrml:resolve($attvalue)"/>
    <xsl:if test="$debug"><xsl:message>RESOLVED <xsl:value-of select="lrml:resolve($attvalue)"/></xsl:message></xsl:if>
  </xsl:template>

  <!-- Strip prefixes from output -->
  <xsl:template match="lrml:hasPrefix"/>
  
  <!-- Copies attributes, elements and text nodes to the output.  -->
  <xsl:template match="node() | @*">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <!-- Copies everything to the transformation output -->
  <xsl:template match="/">
    <xsl:if test="$debug">
      <xsl:message>
        <xsl:for-each select="$prefixes">
          <xsl:value-of select="./@pre"/>:<xsl:value-of select="./@refID"/>
          <xsl:text>&#xa;</xsl:text>
        </xsl:for-each>
      </xsl:message>
    </xsl:if>
    <xsl:copy>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
