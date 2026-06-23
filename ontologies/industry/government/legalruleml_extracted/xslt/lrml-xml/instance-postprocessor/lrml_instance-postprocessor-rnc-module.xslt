<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  >

  <!-- Enforce additional constraint by removing attributes other than xml:id attribute from branch edges. -->
  <xsl:template match="lrml:hasStrength[*]/@*[name()!='xml:id']"/>
  <xsl:template match="lrml:appliesStrength[*]/@*[name()!='xml:id']"/>
  <xsl:template match="lrml:hasQualification[*]/@*[name()!='xml:id']"/>
  <xsl:template match="lrml:hasTemplate[*]/@*[name()!='xml:id']"/>
  
  
</xsl:stylesheet>
