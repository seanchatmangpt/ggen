<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec">
  <!-- Adds an xml:id attribute to every element in the RuleML namespace except ruleml:Data that doesn't already have one -->
  <xsl:template match="ruleml:*[local-name(.)!='Data'][not(@xml:id)]">
    <xsl:copy>
      <xsl:attribute name="xml:id" select="generate-id()"/>
      <xsl:apply-templates select="@*|node()"/>      
    </xsl:copy>
  </xsl:template>
  <!-- Adds an xml:id attribute to every edge element in the LegalRuleML namespace that doesn't already have one -->
  <xsl:template match="lrml:*[matches(local-name(.), '^[a-z]')][not(@xml:id)]">
    <xsl:copy>
      <xsl:attribute name="xml:id" select="generate-id()"/>
      <xsl:apply-templates select="@*|node()"/>      
    </xsl:copy>
  </xsl:template>
  <!-- Copies everything to the transformation output -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
