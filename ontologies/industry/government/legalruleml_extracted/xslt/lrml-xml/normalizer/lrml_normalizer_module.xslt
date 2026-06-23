<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/" xmlns:ruleml="http://ruleml.org/spec"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  >
  <!-- Adapted by OASIS LegalRuleML TC with the permission of RuleML Inc. from http://consumer.ruleml.org/1.02/xslt/normalizer/1.02_normalizer_module.xslt of Consumer RuleML 1.02 -->
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->

  <!-- Functions -->
  <xsl:function name="lrml:isNode" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="(namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and
      matches(local-name($node), '^[A-Z]')) or
      ruleml:isNode($node)
      "
    />
  </xsl:function>

  <xsl:function name="lrml:isComment" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of select="name($node) = 'lrml:Comment'"/>
  </xsl:function>
  
  <xsl:function name="lrml:isParaphrase" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of select="name($node) = 'lrml:Paraphrase'"/>
  </xsl:function>
  
  <xsl:function name="lrml:isStatements" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of select="name($node) = 'lrml:Statements'"/>
  </xsl:function>
  
  <xsl:function name="lrml:isStatement" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="
      namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and
      lrml:isNode($node) and
      matches(local-name($node), 'Statement$')
      "
    />
  </xsl:function>

  <xsl:function name="lrml:isStrength" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/'  and
      (
      local-name($node)='StrictStrength' or
      local-name($node)='DefeasibleStrength' or
      local-name($node)='Defeater'
      )
      "
    />
  </xsl:function>

  <xsl:function name="lrml:isApplicator" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and
      (local-name($node)='Context' or
      local-name($node)='Association' )                
      "
    />
  </xsl:function>

  <xsl:function name="lrml:isCollection" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="
      namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and
      matches(local-name($node), 's$') and
      lrml:isNode($node)
      "
    />
  </xsl:function>

  <xsl:function name="lrml:plural" as="xs:boolean">
    <xsl:param name="member" as="node()"/>
    <xsl:param name="collection" as="node()"/>
    <xsl:variable name="memberName" select="local-name($member)"/>
    <xsl:variable name="collectionName" select="local-name($collection)"/>
    <xsl:value-of
      select="
        matches($collectionName, concat('^', $memberName, 's$')) or
        (matches($memberName,'y$') and
        matches($collectionName, concat('^', substring($memberName, 1, string-length($memberName)-1), 'ies$'))
        )
        "
    />
  </xsl:function>

  <xsl:function name="lrml:isUnwrappedMember" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:param name="parent" as="node()"/>
    <xsl:value-of
      select="
        namespace-uri($parent)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and
        lrml:plural($node, $parent)        
        "
    />
  </xsl:function>

  <xsl:function name="lrml:isDeonticOperation" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="(namespace-uri($node)='http://docs.oasis-open.org/legalruleml/ns/v1.0/' and 
      (
      local-name($node)='Obligation' or
      local-name($node)='Permission' or
      local-name($node)='Prohibition' or
      local-name($node)='Right' or
      local-name($node)='SuborderList'
      )
      )
      "/>

  </xsl:function>
  
  <!-- Modifies the value of the xsi:schemaLocation sttribute-->
  <xsl:template match="@xsi:schemaLocation" mode="phase-1">
    <xsl:attribute name="xsi:schemaLocation" select="replace(., 'compact', 'normal')"/>    
  </xsl:template>

  <!-- Modifies the value of the xml-model processing instruction-->
  <xsl:template match="/processing-instruction('xml-model')" mode="phase-1">
    <xsl:variable name="text"><xsl:value-of select="."/></xsl:variable>
    <xsl:processing-instruction name="xml-model"><xsl:value-of select="replace($text, 'compact', 'normal' )"/></xsl:processing-instruction>
  </xsl:template>
  
  <!-- Wraps the naked children of the LegalRuleML root. -->
  <xsl:template match="lrml:LegalRuleML/*[lrml:isNode(.)][not(lrml:isComment(.))]" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">has<xsl:value-of select="local-name(.)"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- Wraps the naked children of Collections other than Statements. -->
  <xsl:template match="*[lrml:isCollection(.)]/*[lrml:isUnwrappedMember(., ..)]" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">has<xsl:value-of select="local-name(.)"/></xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="lrml:LegalSources/lrml:LegalSources" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasLegalSources</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="lrml:Sources/lrml:Sources" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasSources</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- Wraps the naked children of Statements with a lrml:hasStatement or lrml:hasStatements edge. -->
  <xsl:template match="lrml:Statements/*[lrml:isStatement(.)]" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasStatement</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="lrml:Statements/lrml:Statements" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasStatements</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- Wrap naked children of LegalRuleML Deontic formulas with a ruleml:formula edge -->
  <xsl:template match="lrml:*[lrml:isDeonticOperation(.)]/*[lrml:isNode(.)][not(lrml:isComment(.))][not(lrml:isParaphrase(.))]"
    mode="phase-1">
    <xsl:call-template name="wrap">
      <xsl:with-param name="tag">formula</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- Wrap naked non-comment children of LegalRuleML Statements with a lrml:hasTemplate edge -->
  <xsl:template match="*[lrml:isStatement(.)]/*[lrml:isNode(.)][not(lrml:isComment(.))]" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasTemplate</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- Wrap naked lrml:Paraphrases with a lrml:hasParaphrase edge -->
  <xsl:template match="*[lrml:isNode(.)]/lrml:Paraphrase" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasParaphrase</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- Wrap naked lrml:Comments with a lrml:hasComment edge -->
  <xsl:template match="*[lrml:isNode(.)]/lrml:Comment" mode="phase-1">
    <xsl:call-template name="lrmlwrap">
      <xsl:with-param name="tag">hasComment</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  
  <!-- Wraps the naked LegalRuleML children ( other than Comment and Paraphrase and Strength) of RuleML elements. -->
  <xsl:template
    match="ruleml:*[ruleml:isNode(.)]/lrml:*[lrml:isNode(.)]
    [not(lrml:isComment(.))]
    [not(lrml:isParaphrase(.))]
    [not(lrml:isStrength(.))]
    "
    mode="phase-1">
    <xsl:choose>
      <xsl:when test="local-name(./..)='Neg'">
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">strong</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="local-name(./..)='Naf'">
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">weak</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:when test="local-name(./..)='Equivalent'">
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">torso</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">formula</xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
      
    </xsl:choose>
  </xsl:template>
  
  
  <!-- Named template that wraps an element in the LegalRuleML element given by the tag parameter. -->
  <xsl:template name="lrmlwrap">
    <xsl:param name="tag"/>
    <xsl:element name="lrml:{$tag}">
      <xsl:copy>
        <xsl:apply-templates select="node() | @*" mode="phase-1"/>
      </xsl:copy>
    </xsl:element>
  </xsl:template>

  <!-- Phase III: add required attributes -->
  <!-- Adds the required index attribute to the formula tag in SubOrderList -->
  <xsl:template match="*[self::lrml:SuborderList]/ruleml:formula[not(@index)]" mode="phase-3">
    <xsl:variable name="index_value">
      <xsl:value-of select="count(preceding-sibling::ruleml:formula)+1"/>
    </xsl:variable>
    <xsl:element name="ruleml:formula">
      <xsl:attribute name="index">
        <xsl:value-of select="$index_value"/>
      </xsl:attribute>
      <xsl:apply-templates select="@*|node()" mode="phase-3"/>
    </xsl:element>
  </xsl:template>
  
  <!-- Phase IV: sort by required attributes -->
  
  <!-- Sorts by the required index attribute to the formula tag in SuborderList -->
  <xsl:template match="*[self::lrml:SuborderList]" mode="phase-sort">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="phase-sort"/>
      <xsl:apply-templates select="node()[not(self::ruleml:formula)]" mode="phase-sort"/>
      <xsl:apply-templates select="ruleml:formula" mode="phase-sort">
        <xsl:sort select="@index" data-type="number"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>
  
  

</xsl:stylesheet>
