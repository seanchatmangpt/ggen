<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  >

  <!-- Add the  <?xml version="1.0" ?> at the top of the result.-->
  <xsl:output method="xml" version="1.0" indent="yes"/>
  <xsl:include href="http://deliberation.ruleml.org/1.02/xslt/instance-postprocessor/1.02_instance-postprocessor.xslt"/>

  <xsl:template match="lrml:hasLegalSources[not(*)]">
  </xsl:template>  
  
  <xsl:template match="lrml:hasStatements[not(*)]">
  </xsl:template>  
  
  <xsl:template match="lrml:Figure[count(lrml:hasFunction) = 0]">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:element name="lrml:hasFunction">
        <xsl:attribute name="iri">:</xsl:attribute>
      </xsl:element>
      <xsl:element name="lrml:hasActor">
        <xsl:element name="lrml:Agent"/>
      </xsl:element>      
    </xsl:copy>
  </xsl:template>  
  
  <xsl:template match="ruleml:Data">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="xsi:type">xs:date</xsl:attribute>
      <xsl:text>2015-06-01</xsl:text>
    </xsl:copy>
  </xsl:template> 
  <xsl:template match="lrml:LegalRuleML">
    <xsl:copy>
      <xsl:namespace name="xs" >http://www.w3.org/2001/XMLSchema</xsl:namespace>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
