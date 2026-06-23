<?xml version="1.0" encoding="UTF-8"?>
<!-- Implements all phases of XSLT transformation to RDF/XML except the initial normalization.
     This stylesheet generates valid RDF/XML if the input meets the preconditions.
     There is no loss or ambiguity of information except:
     1. Attributes other than @index or xml:id on edge elements with child or text content will be ignored. This is the desired behavior- ideally, the normalization transformation will not generate such attributes.
     
     Note that xml:id is honored on all elements, even skippable ones.
     
-->
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:lrml="http://docs.oasis-open.org/legalruleml/ns/v1.0/"
  xmlns:lrmlmm="http://docs.oasis-open.org/legalruleml/ns/v1.0/metamodel#"
  xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:ruleml="http://ruleml.org/spec"
  xmlns:rulemm="http://docs.oasis-open.org/legalruleml/ns/v1.0/rule-metamodel#"
  exclude-result-prefixes="#all" xmlns:xi="http://www.w3.org/2003/XInclude"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xsl:variable name="debug" select="false()"/>
  <!--
  Input Preconditions
  1. All CURIEs and CURIE-like abbreviations have been evaluated according to the prefix mapping
     (except the empty prefix)
  2. The document is in the normalized form (e.g. after applying an XSLT normalizer.
  -->

  <!-- Add the  <?xml version="1.0" ?> at the top of the result.-->
  <xsl:output method="xml" version="1.0" indent="yes"/>

  <!-- Functions -->

  <!-- Returns the metamodel prefix for the node-->
  <xsl:function name="lrmlmm:mmIsKnown" as="xs:string">
    <xsl:param name="this" as="item()"/>
    <xsl:choose>
      <xsl:when test="namespace-uri($this)='http://docs.oasis-open.org/legalruleml/ns/v1.0/'">
        <xsl:value-of select="true()"/>
      </xsl:when>
      <xsl:when test="namespace-uri($this)='http://ruleml.org/spec'">
        <xsl:value-of select="true()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="lrmlmm:mmns" as="xs:string">
    <xsl:param name="this" as="item()"/>
    <xsl:choose>
      <xsl:when test="namespace-uri($this)='http://docs.oasis-open.org/legalruleml/ns/v1.0/'">
        <xsl:value-of select="'lrmlmm'"/>
      </xsl:when>
      <xsl:when test="namespace-uri($this)='http://ruleml.org/spec'">
        <xsl:value-of select="'rulemm'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$debug">
          <xsl:message>METAMODEL UNKNOWN</xsl:message>
        </xsl:if>
        <xsl:value-of select="'unknown'"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <!-- Returns the metamodel name for the node-->
  <xsl:function name="lrmlmm:mmname" as="xs:string">
    <xsl:param name="this" as="item()"/>
    <xsl:choose>
      <xsl:when test="lrmlmm:mmIsKnown($this)">
        <xsl:value-of select="concat(lrmlmm:mmns($this), ':', local-name($this))"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="name($this)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <!-- Checks if the item is an edge-->
  <xsl:function name="lrmlmm:isAnyEdge" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:choose>
      <xsl:when test="$this[self::lrml:*][matches(local-name(.), '^[a-z]')]">
        <xsl:value-of select="true()"/>
      </xsl:when>
      <xsl:when
        test="$this[self::ruleml:*][matches(local-name(.), '^[a-z]')][local-name(.)!='slot']">
        <xsl:value-of select="true()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <!-- Checks if the item is a Node-->
  <xsl:function name="lrmlmm:isAnyNode" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:choose>
      <xsl:when test="$this[self::lrml:*][matches(local-name(.), '^[A-Z]')]">
        <xsl:value-of select="true()"/>
      </xsl:when>
      <xsl:when test="$this[self::ruleml:*][matches(local-name(.), '^[A-Z]')]">
        <xsl:value-of select="true()"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="false()"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <!-- Checks if node is a LegalRuleML collection edge
    1. it is an edge in the LegalRuleML namespace
    3. local name starts with 'has'
    4. is in the same namespace as its parent
    5. the suffix of its name (after 'has') matches the local name of the parent
       either exactly or as the singular to the parent plural ('y' -> 'ies').
  -->
  <xsl:function name="lrmlmm:isCollectionEdge" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:variable name="parent" select="$this/.."/>
    <xsl:variable name="type" select="substring-after(local-name($this),'has')"/>
    <xsl:value-of
      select="lrmlmm:isAnyEdge($this) and $this[self::lrml:*] and
      starts-with(local-name($this),'has') and 
      namespace-uri($parent) = namespace-uri($this) and
      ( (concat($type, 's') = local-name($parent)) or 
      ($type = local-name($parent)) or 
      ( ends-with($type, 'y') and (concat(substring($type, 1, string-length($type)-1), 'ies') = local-name($parent)) )
      
      )
      "
    />
  </xsl:function>

  <!-- Checks if node is a RuleML collection edge (has an @index attribute) -->
  <xsl:function name="rulemm:isCollectionEdge" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:value-of select="lrmlmm:isAnyEdge($this) and $this[@index]"/>
  </xsl:function>

  <!-- Checks if node is a LegalRuleML or RuleML collection edge -->
  <xsl:function name="lrmlmm:isAnyCollectionEdge" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:value-of select="lrmlmm:isCollectionEdge($this) or rulemm:isCollectionEdge($this)"/>
  </xsl:function>

  <!-- Checks if node is a LegalRuleML collection that is empty -->
  <xsl:function name="lrmlmm:isEmptyCollection" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:variable name="members" select="$this/*[lrmlmm:isAnyCollectionEdge(.)]"/>
    <xsl:value-of
      select="lrmlmm:isAnyNode($this) and $this[self::lrml:*] and
      matches(local-name($this), 's$') and count($members)=0 and
      count($this/@keyref)=0
      "
    />
  </xsl:function>

  <!-- Checks if node is a RuleML collection owner whose collection is empty -->
  <xsl:function name="rulemm:hasEmptyCollection" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:value-of
      select="rulemm:hasEmptyArgCollection($this) or 
      rulemm:hasEmptyFormulaCollection($this) 
      "
    />
  </xsl:function>

  <xsl:function name="rulemm:hasEmptyArgCollection" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:variable name="members" select="$this/*[lrmlmm:isAnyCollectionEdge(.)]"/>
    <xsl:value-of
      select="lrmlmm:isAnyNode($this) and
      $this[self::ruleml:*] and
      matches(local-name($this), '^Atom$|^Expr$|^Plex$|^Time$|^Spatial$|^Interval$')
      and count($members)=0 and
      count($this/@keyref)=0
      "
    />
  </xsl:function>

  <xsl:function name="rulemm:hasEmptyFormulaCollection" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:variable name="members" select="$this/*[lrmlmm:isAnyCollectionEdge(.)]"/>
    <xsl:value-of
      select="lrmlmm:isAnyNode($this) and
      (
      ($this[self::ruleml:*] and
      matches(local-name($this), '^And$|^Or$|^Operation$'))
      or
      ($this[self::lrml:*] and
      matches(local-name($this), '^SuborderList$'))
      )
      and count($members)=0 and
      count($this/@keyref)=0
      "
    />
  </xsl:function>

  <!-- Check to see if the element is a node that requires keyref merger 
    1. local name must start with capital letter
    2. the node must have a keyref attribute
    3. the node must have at least one child element, text node or attribute that is not keyref
  -->
  <xsl:function name="lrmlmm:merger-node" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:variable name="att-keyref" select="$this/@keyref"/>
    <xsl:variable name="child-elem" select="$this/*"/>
    <xsl:variable name="text" select="$this/text()"/>
    <xsl:variable name="att-not-keyref" select="$this/@*[name(.) != 'keyref']"/>
    <xsl:value-of
      select="matches(local-name($this), '^[A-Z]') and
      count( $att-keyref ) > 0 and count( $child-elem | $text | $att-not-keyref ) > 0"
    />
  </xsl:function>

  <!-- Checks to see if the element is a Reference -->
  <xsl:function name="lrmlmm:isReference" as="xs:boolean">
    <xsl:param name="self" as="node()"/>
    <xsl:value-of select="matches(local-name($self), '^[A-Z].*eference$')"/>
  </xsl:function>

  <!-- Checks to see if the element is a Reference Collection -->
  <xsl:function name="lrmlmm:isReferences" as="xs:boolean">
    <xsl:param name="self" as="node()"/>
    <xsl:value-of select="matches(local-name($self), '^[A-Z].*eferences$')"/>
  </xsl:function>

  <!--makes the node sequence for a collection -->
  <xsl:function name="lrmlmm:makeCollection" as="node()*">
    <xsl:param name="members"/>
    <xsl:choose>
      <xsl:when test="count($members)>0">
        <xsl:variable name="first" select="$members[1]"/>
        <xsl:variable name="edgename" select="name($first)"/>
        <xsl:element name="rdf:first">
          <xsl:apply-templates select="$first/node()"/>
        </xsl:element>
        <xsl:variable name="rest" select="$members[position()>1]"/>
        <xsl:copy-of select="lrmlmm:makeRest($rest)"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="owl:sameAs">
          <xsl:attribute name="rdf:resource"
            >http://www.w3.org/1999/02/22-rdf-syntax-ns#nil</xsl:attribute>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="rulemm:makeCollection" as="node()*">
    <xsl:param name="members"/>
    <xsl:choose>
      <xsl:when test="count($members)>0">
        <xsl:variable name="first" select="$members[1]"/>
        <xsl:variable name="edgename" select="name($first)"/>
        <xsl:element name="{lrmlmm:mmname($first)}s">
          <xsl:element name="rdf:List">
            <xsl:element name="rdf:first">
              <xsl:apply-templates select="$first/node()"/>
            </xsl:element>
            <xsl:variable name="rest" select="$members[position()>1]"/>
            <xsl:copy-of select="lrmlmm:makeRest($rest)"/>
          </xsl:element>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="owl:sameAs">
          <xsl:attribute name="rdf:resource"
            >http://www.w3.org/1999/02/22-rdf-syntax-ns#nil</xsl:attribute>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <xsl:function name="lrmlmm:hasLiteralSymbol" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:value-of
      select="
      $this[self::lrml:*] and
      matches(local-name($this),'^Comment$|^Paraphrse$')
      "
    />
  </xsl:function>


  <!--makes the nodes for the tail of a collection -->
  <xsl:function name="lrmlmm:makeRest" as="item()">
    <xsl:param name="rest"/>
    <xsl:choose>
      <xsl:when test="count($rest) > 0">
        <xsl:variable name="rest-head" select="$rest[1]"/>
        <xsl:element name="rdf:rest">
          <xsl:element name="rdf:List">
            <xsl:element name="rdf:first">
              <xsl:apply-templates select="$rest-head/*"/>
            </xsl:element>
            <xsl:copy-of select="lrmlmm:makeRest(remove($rest, 1))"/>
          </xsl:element>
        </xsl:element>
      </xsl:when>
      <xsl:otherwise>
        <xsl:element name="rdf:rest">
          <xsl:attribute name="rdf:resource"
            >http://www.w3.org/1999/02/22-rdf-syntax-ns#nil</xsl:attribute>
        </xsl:element>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:function>

  <!-- Checks to see if attribute can hold an IRI or CURIE abbreviation -->
  <xsl:function name="lrmlmm:curie-att" as="xs:boolean">
    <xsl:param name="this" as="attribute()"/>
    <xsl:variable name="attname" select="local-name($this)" as="xs:string"/>
    <xsl:value-of
      select="matches($attname, '^memberType$|^type$|^iri$|^refType$|^refIDSystemSource$|^sameAs$|^hasCreationDate$|^strength$|^over$|^under$|^closure$|^style$|^material$')"
    />
  </xsl:function>

  <!-- Default: Copies comments and texts to the transformation output -->
  <xsl:template match="@*">
    <xsl:message>THIS TEMPLATE SHOULD NEVER BE MATCHED: <xsl:value-of select="name(.)"/> =
        <xsl:value-of select="."/></xsl:message>
  </xsl:template>
  <xsl:template match="comment()|text()">
    <xsl:copy-of select="."/>
  </xsl:template>

  <!-- Delete processing instructions -->
  <xsl:template match="processing-instruction()"/>

  <!-- change namespace of elements (default)
       however, process the @key, @xml:id and @keyref attributes first of all attributes
       because they may become attributes, while other attributes become elements
  -->
  <xsl:template match="*">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@key"/>
      <xsl:apply-templates select="@xml:id"/>
      <xsl:apply-templates select="@keyref"/>
      <xsl:apply-templates select="@*[name(.)!='key'][name(.)!='xml:id'][name(.)!='keyref']"/>
      <xsl:apply-templates select="node()"/>
    </xsl:element>
  </xsl:template>

  <!-- Processing the root node:
       1. Wrap in the rdf:RDF root node.
       2. add namespace declarations
       3. write the root element as child 
  -->
  <xsl:template match="/*" priority="100">
    <rdf:RDF>
      <xsl:namespace name="lrmlmm"
        >http://docs.oasis-open.org/legalruleml/ns/v1.0/metamodel#</xsl:namespace>
      <xsl:namespace name="owl">http://www.w3.org/2002/07/owl#</xsl:namespace>
      <xsl:namespace name="rdfs">http://www.w3.org/2000/01/rdf-schema#</xsl:namespace>
      <xsl:namespace name="rulemm"
        >http://docs.oasis-open.org/legalruleml/ns/v1.0/rule-metamodel#</xsl:namespace>
      <xsl:copy-of select="descendant::*/namespace::*"/>
      <xsl:copy-of select="@xml:base"/>
      <xsl:choose>
        <xsl:when test="lrmlmm:merger-node(.)">
          <xsl:call-template name="merger">
            <xsl:with-param name="this" select="."/>
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          <xsl:element name="{lrmlmm:mmname(.)}">
            <xsl:apply-templates select="@key"/>
            <xsl:apply-templates select="@*[name(.)!='xml:base'][name(.)!='key']|node()"/>
          </xsl:element>
        </xsl:otherwise>
      </xsl:choose>
    </rdf:RDF>
  </xsl:template>

  <!-- Qualified attributes -->
  <!-- Remove the hint to the location of the XSD schema, as this will not be
       applicable to the output. -->
  <xsl:template match="@xsi:schemaLocation" priority="10"/>
  <xsl:template match="@xml:base" priority="10"/>

  <!-- change @xsi:type to @rdf:datatype -->
  <xsl:template match="@xsi:type" priority="10">
    <xsl:attribute name="rdf:datatype">
      <xsl:variable name="val">
        <xsl:value-of select="."/>
      </xsl:variable>
      <xsl:variable name="local" select="substring-after($val, ':')"/>
      <xsl:variable name="prefix" select="substring-before($val, ':')"/>
      <xsl:variable name="ns" select="namespace-uri-for-prefix($prefix, ./..)"/>
      <xsl:value-of select="concat($ns,'#', $local)"/>
    </xsl:attribute>
  </xsl:template>


  <!-- change @xml:id to @rdf:ID on any element in the lrml namespace -->
  <xsl:template match="lrml:*/@xml:id" priority="50">
    <xsl:attribute name="rdf:ID">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <!-- change @xml:id to @rdf:ID on any edge element in the ruleml namespace -->
  <xsl:template match="ruleml:*[matches(local-name(), '^[a-z]')]/@xml:id" priority="50">
    <xsl:attribute name="rdf:ID">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <!-- change @xml:id to @rulemm:ID on any Node element in the ruleml namespace -->
  <xsl:template match="ruleml:*[matches(local-name(), '^[A-Z]')]/@xml:id" priority="50">
    <xsl:element name="rulemm:ID">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="concat('#', .)"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>


  <!-- Unqualified attributes -->
  <!-- rename attributes -->
  <!-- default: add metamodel prefix to unqualified attributes according to the parent -->
  <!-- This template should be applied only to attributes that do not satisfy the function lrmlmm:curie-att-->
  <xsl:template match="@*[namespace-uri(.)='']" priority="3">
    <xsl:attribute name="{lrmlmm:mmns(./..)}:{local-name(.)}">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <!-- exception: attribute with IRI or empty-prefix CURIE value become property elements with modified value -->
  <xsl:template match="@*[lrmlmm:curie-att(.)]" priority="9">
    <xsl:element name="{lrmlmm:mmns(./..)}:{local-name(.)}">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="replace(., '^:', '#')"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- Specific attribute exceptions, possibly constrained by namespace of parent -->
  <!-- exception: change @sameAs to @owl:sameAs -->
  <xsl:template match="@sameAs" priority="10">
    <xsl:element name="owl:sameAs">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="replace(., '^:', '#')"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- exception: change @key to @rdf:ID if parent is lrml:* -->
  <xsl:template match="lrml:*/@key" priority="10">
    <xsl:attribute name="rdf:ID">
      <xsl:value-of select="."/>
    </xsl:attribute>
  </xsl:template>

  <!-- exception: change @key to @rdf:about if parent is ruleml:* -->
  <xsl:template match="ruleml:*/@key" priority="10">
    <xsl:attribute name="rdf:about">
      <xsl:value-of select="replace(., '^:', '#')"/>
    </xsl:attribute>
  </xsl:template>

  <!-- exception: type on lrml:* element becomes rdf:type -->
  <xsl:template match="lrml:*/@type" priority="10">
    <xsl:element name="rdf:type">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="replace(., '^:', '#')"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- exception: refID becomes an edge with text content -->
  <xsl:template match="@refID" priority="10">
    <xsl:element name="lrmlmm:refID">
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>

  <!-- exception: refIDSystemName becomes an edge with text content -->
  <xsl:template match="@refIDSystemName" priority="10">
    <xsl:element name="lrmlmm:refIDSystemName">
      <xsl:value-of select="."/>
    </xsl:element>
  </xsl:template>

  <!-- exception: lrml:*/refersTo has a more complex expansion.
        add an lrmlmm:refersTo *edge* element containing
           a rdf:Description element
        The rdf:Description element has an rdf:about attribute with value obtained from
         the @refersTo attribute
         The rdf:Description element also contains the
             application of templates to other attributes and children of the
             parent of @refersTo.        
  -->
  <xsl:template match="lrml:*/@refersTo" priority="10">
    <xsl:element name="lrmlmm:refersTo">
      <xsl:element name="rdf:Description">
        <xsl:attribute name="rdf:ID" select="."/>
        <xsl:apply-templates
          select="parent::*/@*[name(.)!='refersTo'][name(.)!='refID'][name(.)!='refIDSystemName']"/>
        <xsl:apply-templates select="parent::*/@refID"/>
        <xsl:apply-templates select="parent::*/@refIDSystemName"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- Specific attribute exceptions with constraint on type and/or content of parent -->
  <!-- exception: change keyref on edge to rdf:resource if no children or text -->
  <xsl:template match="*[lrmlmm:isAnyEdge(.)][count(*|./text()) = 0]/@keyref" priority="11">
    <xsl:attribute name="rdf:resource">
      <xsl:value-of select="replace(., '^:', '#')"/>
    </xsl:attribute>
  </xsl:template>

  <!-- exception: ignore @keyref iff there are children or text nodes of an edge -->
  <xsl:template match="*[lrmlmm:isAnyEdge(.)][count(*|./text()) > 0]/@keyref" priority="11"/>

  <!-- exception: A @keyref attribute on any Node becomes rdf:about attribute -->
  <xsl:template match="*[lrmlmm:isAnyNode(.)]/@keyref" priority="11">
    <xsl:attribute name="rdf:about">
      <xsl:value-of select="replace(., '^:', '#')"/>
    </xsl:attribute>
  </xsl:template>

  <!-- exception: iri becomes rdf:resource on edge elements with no children or text nodes -->
  <xsl:template match="*[lrmlmm:isAnyEdge(.)][count(*|./text()) = 0]/@iri" priority="11">
    <xsl:attribute name="rdf:resource">
      <xsl:value-of select="replace(., '^:', '#')"/>
    </xsl:attribute>
  </xsl:template>

  <!-- exception: ignore @iri iff there are children or text nodes of an edge -->
  <xsl:template match="*[lrmlmm:isAnyEdge(.)][count(*|./text()) > 0]/@iri" priority="11"/>

  <!-- exception: change iri on a Node to owl:sameAs -->
  <xsl:template match="*[lrmlmm:isAnyNode(.)]/@iri" priority="11">
    <xsl:element name="owl:sameAs">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="replace(., '^:', '#')"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- exception: refType on anything except lrml:References becomes rdf:type -->
  <xsl:template match="lrml:*[not(lrmlmm:isReferences(.))]/@refType" priority="11">
    <xsl:element name="rdf:type">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="replace(., '^:', '#')"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- exception: refType on lrml:References because memberReferenceType -->
  <xsl:template match="lrml:*[lrmlmm:isReferences(.)]/@refType" priority="11">
    <xsl:element name="lrmlmm:memberReferenceType">
      <xsl:attribute name="rdf:resource">
        <xsl:value-of select="replace(., '^:', '#')"/>
      </xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- exception: Don't copy the index attribute on edges -->
  <xsl:template match="*[lrmlmm:isAnyEdge(.)]/@index" priority="11"/>

  <!-- edge exceptions -->

  <xsl:template match="*[lrmlmm:isAnyCollectionEdge(.)]" priority="8"/>

  <!-- First member of a collection has a pull template -->
  <xsl:template match="*[lrmlmm:isCollectionEdge( . )][1]" priority="9">
    <xsl:copy-of select="lrmlmm:makeCollection(../*[lrmlmm:isCollectionEdge( . )])"/>
  </xsl:template>

  <xsl:template match="*[rulemm:isCollectionEdge( . )][1]" priority="9">
    <xsl:copy-of select="rulemm:makeCollection(../*[rulemm:isCollectionEdge( . )])"/>
  </xsl:template>

  <!-- Non-first edge elements of a collection have empty template because they are pulled. -->
  <xsl:template match="*[lrmlmm:isAnyCollectionEdge( . )]
    [position() > 1]" priority="9"/>

  <!-- ruleml:slot gets a parseType="Collection" attribute -->
  <xsl:template match="ruleml:slot" priority="10">
    <xsl:element name="rulemm:slot">
      <xsl:attribute name="rdf:parseType" select="'Collection'"/>
      <xsl:apply-templates select="@*"/>
      <xsl:apply-templates select="node()"/>
    </xsl:element>
  </xsl:template>


  <!-- Node exceptions -->
  <!-- Comment and Paraphrase Node with child or text node has a more complex expansion.
         apply templates to attributes, keyref last
         add symbol edge with parseType of Literal
         apply templates to nodes
  -->

  <xsl:function name="lrmlmm:isLiteralNode" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:value-of select="lrmlmm:hasLiteralSymbol($this) and count($this/(*|text())) >0"/>
  </xsl:function>

  <xsl:template match="*[lrmlmm:isLiteralNode(.)]" priority="9">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@key"/>
      <xsl:apply-templates select="@xml:id"/>
      <xsl:apply-templates select="@keyref"/>
      <xsl:apply-templates select="@*[name(.)!='key'][name(.)!='xml:id'][name(.)!='keyref']"/>
      <xsl:call-template name="LiteralNode">
        <xsl:with-param name="this" select="."/>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>

  <xsl:template name="LiteralNode">
    <xsl:param name="this" as="node()"/>
    <xsl:element name="lrmlmm:symbol">
      <xsl:attribute name="rdf:parseType" select="'Literal'"/>
      <xsl:apply-templates select="$this/node()"/>
    </xsl:element>
  </xsl:template>

  <!-- lrml:*Reference has a more complex expansion.
       The prefix is changed to lrmlmm, as usual.
       However, only the @refersTo attribute and nodes are matched directly.
  -->
  <xsl:template match="lrml:*[lrmlmm:isReference(.)]" priority="9">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@refersTo"/>
      <xsl:apply-templates select="node()"/>
    </xsl:element>
  </xsl:template>

  <!-- ruleml leaf Nodes have a more complex expansion.
    A rulemm:symbol edge is inserted, which carries the datatype information if there is any.
    That is, an xsi:type attribute is renamed to rdf:datatype and
    added to the rulemm:symbol edge.
    -->

  <xsl:function name="lrmlmm:isLeafNode" as="xs:boolean">
    <xsl:param name="this" as="node()"/>
    <xsl:value-of
      select="matches(local-name($this), '^Data$|^Ind$|^Var$|^Skolem$|^Rel$|^Fun$') and namespace-uri($this)='http://ruleml.org/spec'"
    />
  </xsl:function>

  <xsl:template match="ruleml:*[matches(local-name(.), '^Data$|^Ind$|^Var$|^Skolem$|^Rel$|^Fun$')]"
    priority="10">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@key"/>
      <xsl:apply-templates select="@xml:id"/>
      <xsl:apply-templates select="@keyref"/>
      <xsl:apply-templates
        select="@*[name(.)!='key'][name(.)!='keyref'][name(.)!='xml:id'][name(.)!='keyref'][name(.)!='xsi:type']"/>
      <xsl:call-template name="LeafNode">
        <xsl:with-param name="this" select="."/>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>

  <xsl:template name="LeafNode">
    <xsl:param name="this" as="node()"/>
    <xsl:element name="rulemm:symbol">
      <xsl:if test="$this/*">
        <xsl:attribute name="rdf:parseType">Literal</xsl:attribute>
      </xsl:if>
      <xsl:apply-templates select="$this/@xsi:type"/>
      <xsl:apply-templates select="$this/node()"/>
    </xsl:element>
  </xsl:template>


  <!-- Merger Nodes have a more complex expansion
    -->
  <xsl:template match="*[lrmlmm:merger-node(.)]" priority="20">
    <xsl:call-template name="merger">
      <xsl:with-param name="this" select="."/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="merger">
    <xsl:param name="this" as="node()"/>
    <xsl:element name="rdfs:Resource">
      <xsl:apply-templates select="$this/@*[name(.)='key']"/>
      <xsl:element name="lrmlmm:mergerOf">
        <xsl:attribute name="rdf:parseType" select="'Collection'"/>
        <xsl:element name="rdfs:Resource">
          <xsl:apply-templates select="$this/@*[name(.)='keyref']"/>
        </xsl:element>
        <xsl:element name="{lrmlmm:mmname(.)}">
          <xsl:apply-templates select="$this/@xml:id"/>
          <xsl:apply-templates
            select="$this/@*[name(.)!='keyref'][name(.)!='key'][name(.)!='xml:id']"/>
          <xsl:choose>
            <xsl:when test="lrmlmm:isLiteralNode($this)">
              <xsl:call-template name="LiteralNode">
                <xsl:with-param name="this" select="$this"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:when test="lrmlmm:isLeafNode($this)">
              <xsl:call-template name="LeafNode">
                <xsl:with-param name="this" select="$this"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:when test="lrmlmm:isEmptyCollection($this)">
              <xsl:call-template name="EmptyCollection">
                <xsl:with-param name="this" select="$this"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:when test="rulemm:hasEmptyArgCollection($this)">
              <xsl:call-template name="EmptyArgCollection">
                <xsl:with-param name="this" select="$this"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:when test="rulemm:hasEmptyFormulaCollection($this)">
              <xsl:call-template name="EmptyFormulaCollection">
                <xsl:with-param name="this" select="$this"/>
              </xsl:call-template>
            </xsl:when>
            <xsl:otherwise>
              <xsl:apply-templates select="$this/node()"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <!-- Anything inside lrml:Comment or lrml:Paraphrase or ruleml:Data -->

  <xsl:template
    match="*[ancestor::lrml:Comment or ancestor::lrml:Paraphrase or ancestor::ruleml:Data]/@*"
    priority="100">
    <xsl:copy/>
  </xsl:template>

  <xsl:template
    match="node()[ancestor::lrml:Comment or ancestor::lrml:Paraphrase or ancestor::ruleml:Data]"
    priority="100">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

  <!-- Empty collections have an exceptional pattern -->
  <xsl:template match="*[lrmlmm:isEmptyCollection(.)]">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@key"/>
      <xsl:apply-templates select="@xml:id"/>
      <xsl:apply-templates select="@keyref"/>
      <xsl:apply-templates select="@*[name(.)!='key'][name(.)!='xml:id'][name(.)!='keyref']"/>
      <xsl:call-template name="EmptyCollection">
        <xsl:with-param name="this" select="."/>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>

  <xsl:template name="EmptyCollection">
    <xsl:param name="this" as="node()"/>
    <xsl:apply-templates select="$this/node()"/>
    <xsl:element name="lrmlmm:hasMembers">
      <xsl:attribute name="rdf:resource"
        >http://www.w3.org/1999/02/22-rdf-syntax-ns#nil</xsl:attribute>
    </xsl:element>
  </xsl:template>

  <xsl:template match="*[rulemm:hasEmptyArgCollection(.)]">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@key"/>
      <xsl:apply-templates select="@xml:id"/>
      <xsl:apply-templates select="@keyref"/>
      <xsl:apply-templates select="@*[name(.)!='key'][name(.)!='xml:id'][name(.)!='keyref']"/>
      <xsl:call-template name="EmptyArgCollection">
        <xsl:with-param name="this" select="."/>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>

  <xsl:template name="EmptyArgCollection">
    <xsl:param name="this" as="node()"/>
    <xsl:apply-templates select="$this/node()"/>
    <xsl:element name="rulemm:args">
      <xsl:attribute name="rdf:resource"
        >http://www.w3.org/1999/02/22-rdf-syntax-ns#nil</xsl:attribute>
    </xsl:element>
  </xsl:template>

  <xsl:template match="*[rulemm:hasEmptyFormulaCollection(.)]">
    <xsl:element name="{lrmlmm:mmname(.)}">
      <xsl:apply-templates select="@key"/>
      <xsl:apply-templates select="@xml:id"/>
      <xsl:apply-templates select="@keyref"/>
      <xsl:apply-templates select="@*[name(.)!='key'][name(.)!='xml:id'][name(.)!='keyref']"/>
      <xsl:call-template name="EmptyFormulaCollection">
        <xsl:with-param name="this" select="."/>
      </xsl:call-template>
    </xsl:element>
  </xsl:template>

  <xsl:template name="EmptyFormulaCollection">
    <xsl:param name="this" as="node()"/>
    <xsl:apply-templates select="$this/node()"/>
    <xsl:element name="rulemm:formulas">
      <xsl:attribute name="rdf:resource"
        >http://www.w3.org/1999/02/22-rdf-syntax-ns#nil</xsl:attribute>
    </xsl:element>
  </xsl:template>

  <!-- hasTemplate with child has a more complex expansion.
       Assumptions: has no @keyref, or ignore it if it exists
       Rename the edge to lrmlmm:hasTemplate containing
         an lrmlmm:Template element.
       The lrmlmm:Template element contains
         the application of templates matching an @key attribute on its child.
       That is, the @key attribute of the template root becomes the identifier
         of the lrmlmm:Template.
       The lrmlmm:Template element also contains a lrmlmm:symbol edge element
          with parseType of Literal, and contents literally copied
          from the original lrml:hasTemplate element.       
  -->
  <xsl:template match="lrml:hasTemplate[ruleml:*]" priority="11">
    <xsl:element name="lrmlmm:hasTemplate">
      <xsl:variable name="template-root" select="./*"/>
      <xsl:variable name="haskey" select="count($template-root/@key)"/>
      <xsl:variable name="template-key" select="$template-root/@key"/>
      <xsl:variable name="vApos">'</xsl:variable>
      <xsl:variable name="xp"
        select="concat('#xpointer(//ruleml:',
                       local-name($template-root),
                       '[key=', $vApos, $template-key, $vApos,
                       '])')"> </xsl:variable>
      <xsl:element name="lrmlmm:Template">
        <xsl:if test="$haskey">
          <xsl:attribute name="rdf:about" select="concat('#', substring($template-key,2))"/>
        </xsl:if>
        <xsl:element name="lrmlmm:symbol">
          <xsl:attribute name="rdf:parseType" select="'Literal'"/>
          <xsl:element name="xi:include">
            <xsl:attribute name="href" select="$xp"/>
            <xsl:attribute name="xml:base" select="base-uri()"/>
          </xsl:element>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="lrml:hasTemplate[lrml:*]" priority="11">
    <xsl:element name="lrmlmm:hasTemplate">
      <xsl:variable name="template-root" select="./*"/>
      <xsl:variable name="haskey" select="count($template-root/@key)"/>
      <xsl:variable name="template-key" select="$template-root/@key"/>
      <xsl:variable name="vApos">'</xsl:variable>
      <xsl:variable name="xp"
        select="concat('#xpointer(//lrml:',
        local-name($template-root),
        '[key=', $vApos, $template-key, $vApos,
        '])')"> </xsl:variable>
      <xsl:element name="lrmlmm:Template">
        <xsl:if test="$haskey">
          <xsl:attribute name="rdf:ID" select="$template-key"/>
        </xsl:if>
        <xsl:element name="lrmlmm:symbol">
          <xsl:attribute name="rdf:parseType" select="'Literal'"/>
          <xsl:element name="xi:include">
            <xsl:attribute name="href" select="$xp"/>
            <xsl:attribute name="xml:base" select="base-uri()"/>
          </xsl:element>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template match="lrml:hasQualification[ruleml:*]" priority="11">
    <xsl:element name="lrmlmm:hasQualification">
      <xsl:variable name="template-root" select="./*"/>
      <xsl:variable name="haskey" select="count($template-root/@key)"/>
      <xsl:variable name="template-key" select="$template-root/@key"/>
      <xsl:variable name="vApos">'</xsl:variable>
      <xsl:variable name="xp"
        select="concat('#xpointer(//ruleml:',
        local-name($template-root),
        '[key=', $vApos, $template-key, $vApos,
        '])')"> </xsl:variable>
      <xsl:element name="lrmlmm:Qualification">
        <xsl:if test="$haskey">
          <xsl:attribute name="rdf:about" select="concat('#', substring($template-key,2))"/>
        </xsl:if>
        <xsl:element name="lrmlmm:symbol">
          <xsl:attribute name="rdf:parseType" select="'Literal'"/>
          <xsl:element name="xi:include">
            <xsl:attribute name="href" select="$xp"/>
            <xsl:attribute name="xml:base" select="base-uri()"/>
          </xsl:element>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>

  <xsl:template
    match="lrml:hasQualification[lrml:*[name(.)!='lrml:Override'][name(.)!='lrml:Reparation']]"
    priority="11">
    <xsl:element name="lrmlmm:hasQualification">
      <xsl:variable name="template-root" select="./*"/>
      <xsl:variable name="haskey" select="count($template-root/@key)"/>
      <xsl:variable name="template-key" select="$template-root/@key"/>
      <xsl:variable name="vApos">'</xsl:variable>
      <xsl:variable name="xp"
        select="concat('#xpointer(//lrml:',
        local-name($template-root),
        '[key=', $vApos, $template-key, $vApos,
        '])')"> </xsl:variable>
      <xsl:element name="lrmlmm:Qualification">
        <xsl:if test="$haskey">
          <xsl:attribute name="rdf:ID" select="$template-key"/>
        </xsl:if>
        <xsl:element name="lrmlmm:symbol">
          <xsl:attribute name="rdf:parseType" select="'Literal'"/>
          <xsl:element name="xi:include">
            <xsl:attribute name="href" select="$xp"/>
            <xsl:attribute name="xml:base" select="base-uri()"/>
          </xsl:element>
        </xsl:element>
      </xsl:element>
    </xsl:element>
  </xsl:template>


</xsl:stylesheet>
