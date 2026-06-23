<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/" xmlns:ruleml="http://ruleml.org/spec">

  <xsl:include
    href="http://deliberation.ruleml.org/1.02/xslt/instance-postprocessor/1.02_instance-postprocessor.xslt"/>

  <xsl:template match="lrml:Figure[count(lrml:hasFunction) = 0 ]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="lrml:hasFunction">
        <xsl:attribute name="iri">:</xsl:attribute>
      </xsl:element>
      <xsl:element name="lrml:Agent"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template
    match="lrml:*[matches(local-name(.), '^has')]
    [count(*) = 0]
    [local-name(.)!='hasStrength']
    [local-name(.)!='hasQualification']
    [local-name(.)!='hasMemberType']
    [local-name(.)!='hasType']
    [local-name(.)!='hasActor']
    [local-name(.)!='hasFunction']
    [local-name(.)!='hasStatusDevelopment']
    [local-name(.)!='hasAlternative']
    ">
    <xsl:variable name="childname" select="concat('lrml:', substring(local-name(.), 4,
      string-length(local-name(.))-3))"/>
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="{resolve-QName($childname, .)}"/>
      
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
