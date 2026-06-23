<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:ruleml="http://ruleml.org/spec" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <!-- dc:rights [ 'Copyright 2015 RuleML Inc. - Licensed under the RuleML Specification License, Version 1.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://ruleml.org/licensing/RSL1.0-RuleML. Disclaimer: THIS SPECIFICATION IS PROVIDED "AS IS" AND ANY EXPRESSED OR IMPLIED WARRANTIES, ..., EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. See the License for the specifics governing permissions and limitations under the License.' ] -->

  <!-- Functions -->
  <!-- test for elements that denote temporal, spatial or interval entities-->
  <xsl:function name="ruleml:isTSINode" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://ruleml.org/spec' and (
      local-name($node)='Time' or
      local-name($node)='Spatial' or
      local-name($node)='Interval'
      )
      "
    />
  </xsl:function>

  <!-- test for elements that denote generic sentences-->
  <xsl:function name="ruleml:isGenericSentence" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://ruleml.org/spec' and 
      (local-name($node)='Operation' or
      local-name($node)='Negation')
      "
    />
  </xsl:function>

  <!-- test for elements that contruct temporal, spatial or interval entities from other temporal, spatial or interval entities-->
  <xsl:function name="ruleml:isIntervalConstructor" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://ruleml.org/spec' and
      (local-name($node)='After' or
      local-name($node)='Before' or
      local-name($node)='Every' or
      local-name($node)='Any' or
      local-name($node)='Timer'
      )
      "
    />
  </xsl:function>

  <!-- test for elements that denote the application of Allen operators-->
  <xsl:function name="ruleml:isAllenOperation" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://ruleml.org/spec' and
      (local-name($node)='Precedes' or
      local-name($node)='Meets' or
      local-name($node)='Overlaps' or
      local-name($node)='Starts' or
      local-name($node)='During' or
      local-name($node)='Finishes' or
      local-name($node)='Succeeds'
      )
      "
    />
  </xsl:function>

  <xsl:function name="ruleml:isRule" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="namespace-uri($node)='http://ruleml.org/spec' and
      local-name($node)='Rule'
      "
    />
  </xsl:function>

  <!-- A test for elements that can only hold terms as Node children, not Formulas -->
  <xsl:function name="ruleml:isTermHolder" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="
      ruleml:isAEPNode($node) or
      ruleml:isTSINode($node) or
      ruleml:isIntervalConstructor($node) or
      ruleml:isTermEdge($node)         
      "
    />
  </xsl:function>
  <xsl:function name="ruleml:isFormulaHolder" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:value-of
      select="
      ruleml:isCompoundSentence($node) or
      ruleml:isRule($node) or
      ruleml:isFormulaEdge($node)      
      "
    />
  </xsl:function>

  <xsl:function name="ruleml:isCompoundTerm" as="xs:boolean">
    <xsl:param name="node" as="node()"/>
    <xsl:param name="nodeParent"/>
    <xsl:value-of
      select="(namespace-uri($node)='http://ruleml.org/spec' and
      (local-name($node)='Expr' or
      local-name($node)='Plex')) or
      (ruleml:isTSINode($node) and ruleml:isTermHolder($nodeParent)) or
      ruleml:isIntervalConstructor($node)            
      "
    />
  </xsl:function>

  <!-- Phase I: insert stripes if skipped -->

  <!-- Wraps the second to last RuleML child of Rule. -->
  <xsl:template
    match="ruleml:Rule/*[namespace-uri(.)='http://ruleml.org/spec' and position()=last()-1]"
    mode="phase-1">
    <!--<xsl:comment>second to last</xsl:comment>-->
    <xsl:choose>
      <xsl:when test="local-name()='if' or local-name()='then'">
        <xsl:call-template name="copy-1"/>
      </xsl:when>
      <xsl:when test="local-name(following-sibling::*[1])='if'">
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">then</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">if</xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Wraps the last RuleML child of Rule -->
  <xsl:template
    match="ruleml:Rule/*[namespace-uri(.)='http://ruleml.org/spec' and position()=last()]"
    mode="phase-1">
    <!--<xsl:comment>last</xsl:comment>-->
    <xsl:choose>
      <xsl:when test="local-name()='if' or local-name()='then'">
        <xsl:call-template name="copy-1"/>
      </xsl:when>
      <xsl:when test="local-name(preceding-sibling::*[1])='then'">
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">if</xsl:with-param>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="wrap">
          <xsl:with-param name="tag">then</xsl:with-param>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- Wraps the naked children of Time, Spatial and Interval. -->
  <xsl:template match="*[ruleml:isTSINode(.)]/*[ruleml:isNode(.)]" mode="phase-1">
    <xsl:call-template name="wrap">
      <xsl:with-param name="tag">arg</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- Wraps the naked RuleML children of generics Operation and Negation.-->
  <xsl:template match="*[ruleml:isGenericSentence(.)]/*[
    ruleml:isNode(.)]" mode="phase-1">
    <xsl:call-template name="wrap">
      <xsl:with-param name="tag">formula</xsl:with-param>
    </xsl:call-template>
  </xsl:template>


  <!-- Wraps the naked RuleML children of Allen operators.-->
  <xsl:template match="*[ruleml:isAllenOperation(.)]/*[ruleml:isNode(.)]" mode="phase-1">
    <xsl:call-template name="wrap">
      <xsl:with-param name="tag">arg</xsl:with-param>
    </xsl:call-template>
  </xsl:template>

  <!-- Wraps the naked RuleML children of interval constructors.-->
  <xsl:template match="*[ruleml:isIntervalConstructor(.)]/*[ruleml:isNode(.)]" mode="phase-1">
    <xsl:call-template name="wrap">
      <xsl:with-param name="tag">arg</xsl:with-param>
    </xsl:call-template>
  </xsl:template>


  <!-- Phase II: rearrange into canonical ordering -->


  <!-- Builds canonically-ordered content of Time, Spatial, Interval. -->
  <xsl:template match="*[ruleml:isTSINode(.)]" mode="phase-2">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="phase-2"/>
      <xsl:apply-templates select="comment()" mode="phase-2"/>
      <xsl:apply-templates select="*[namespace-uri(.)!='http://ruleml.org/spec']" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:meta" mode="phase-2"/>
      <xsl:apply-templates select="*[namespace-uri(.)='http://ruleml.org/spec' and
        not (
        local-name() = 'meta' or 
        local-name() = 'oid' or 
        local-name() = 'degree' or 
        local-name() = 'op' or 
        local-name() = 'arg' or 
        local-name() = 'content' or 
        local-name() = 'repo' or 
        local-name() = 'slot' or 
        local-name() = 'resl'
        )]" 
        mode="phase-2"/>
      <xsl:apply-templates select="ruleml:oid" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:degree" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:op" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:*[local-name() = 'arg' or 
        local-name() = 'content']" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:repo" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:slot" mode="phase-2"/>
      <xsl:apply-templates select="ruleml:resl" mode="phase-2"/>
    </xsl:copy>
  </xsl:template>

  <!-- Phase III: add required attributes -->
  <!-- Adds the required index attribute to the formula tag in Operation -->
  <xsl:template match="*[self::ruleml:Operation]/ruleml:formula[not(@index)]" mode="phase-3">
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

  <!-- Sorts by the required index attribute to the formula tag in Operation -->
  <xsl:template match="*[self::ruleml:Operation]" mode="phase-sort">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="phase-sort"/>
      <xsl:apply-templates select="node()[not(self::ruleml:formula)]" mode="phase-sort"/>
      <xsl:apply-templates select="ruleml:formula" mode="phase-sort">
        <xsl:sort select="@index" data-type="number"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>


  <!-- Sorts by the required index attribute to the arg and content tag -->
  <xsl:template match="*[ruleml:content]" mode="phase-sort">
    <xsl:copy>
      <xsl:apply-templates select="@*" mode="phase-sort"/>
      <xsl:apply-templates select="node()[not(self::ruleml:content or self::ruleml:content)]" mode="phase-sort"/>
      <xsl:apply-templates select="*[self::ruleml:content or self::ruleml:content]" mode="phase-sort">
        <xsl:sort select="@index" data-type="number"/>
      </xsl:apply-templates>
    </xsl:copy>
  </xsl:template>

  <!-- Pretty Print -->


</xsl:stylesheet>
