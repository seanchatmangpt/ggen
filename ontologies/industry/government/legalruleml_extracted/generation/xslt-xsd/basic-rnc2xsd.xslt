<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="#all">
  <xsl:include href="rnc2xsd_module.xslt"/>

  <xsl:template match="xs:schema[@targetNamespace='http://ruleml.org/spec']">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xs:import namespace="http://www.w3.org/XML/1998/namespace"
        schemaLocation="http://www.w3.org/2009/01/xml.xsd"/>
      <xs:import namespace="http://docs.oasis-open.org/legalruleml/ns/v1.0/" schemaLocation="lrml-basic.xsd"/>
      <xs:include
        schemaLocation="../datatypes/SimpleWithAttributes.xsd"/>
      <xsl:apply-templates select="text()|processing-instruction()|*[ name()!='xs:import']"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template
    match="xs:schema[@targetNamespace='http://docs.oasis-open.org/legalruleml/ns/v1.0/']">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xs:import namespace="http://www.w3.org/XML/1998/namespace"
        schemaLocation="http://www.w3.org/2009/01/xml.xsd"/>
      <xs:import namespace="http://ruleml.org/spec" schemaLocation="ruleml.xsd"/>
      <xsl:apply-templates select="text()|processing-instruction()|*[ name()!='xs:import']"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
