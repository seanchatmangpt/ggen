<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:ruleml="http://ruleml.org/spec">
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->
  <!-- dc:description [ 'Transformation to enforce the additional constraint
       of no key and keyref attributes on branch edges, 
       while also reporting such changes via xsl:message.
       Target schema is normalized serialization. ' ] -->
  
  <xsl:function name="lrml:isAnyEdge" as="xs:boolean">
    <xsl:param name="this" as="item()"/>
    <xsl:value-of select="matches(local-name($this), '^[a-z]') and 
      ($this[self::lrml:*] or $this[self::ruleml:*]) and not(name($this)='ruleml:slot')"/>
  </xsl:function>

  <xsl:template match="*[lrml:isAnyEdge(.)][*]/@*[name()!='xml:id'][name()!='index']">
    <xsl:variable name="edge" select="parent::*"/>
    <xsl:message>INVALID ATTRIBUTE on <xsl:value-of select="name($edge)"/>: edges with children should not have attributes other than xml:id"</xsl:message>
    </xsl:template>
  
  <!-- Copies everything else to the transformation output -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
