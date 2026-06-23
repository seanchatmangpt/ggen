<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema" exclude-result-prefixes="#all">
  
  <!-- Remove almost all white space between elements -->
  <xsl:strip-space elements="*"/>
  
  <!-- Add the  <?xml version="1.0" ?> at the top of the result.-->
  <xsl:output method="xml" version="1.0" indent="yes" exclude-result-prefixes="xs"/>
  
  <!-- Fix elements with wild-card content. -->
  <xsl:template match="xs:element[@name='Data']
    [./ancestor::node()
      [name()='xs:group']
      [@*[name(.)='name'][data(.)='Data_any.Node.def']
    ]]">
    <xs:element name="Data" type="xs:anyType"/>
  </xsl:template>
  <xsl:template match="xs:complexType[@name='Data_any.type.def']"/>    
  
  <xsl:template match="xs:complexType[@name='anycontent.content']">
    <xs:complexType mixed="true" name="anycontent.content">
      <xs:sequence>
        <xs:any maxOccurs="unbounded" minOccurs="0" processContents="lax"/>
      </xs:sequence>
    </xs:complexType>
  </xsl:template>

  <xsl:template match=" xs:element[@name='Comment']/xs:complexType | xs:element[@name='Paraphrase']/xs:complexType">
    <xsl:copy>
      <xsl:apply-templates select="@*"/>
      <xs:sequence>
        <xs:any minOccurs="0" processContents="skip"/>
      </xs:sequence>
      <xsl:apply-templates select="xs:attributeGroup"/>      
    </xsl:copy>
  </xsl:template>
  
    <!-- Fix elements with required index attribute. -->
  <xsl:template
    match="*[not (@name='formula_And-datt.choice') and not (@name='formula_Or-datt.choice')]/xs:attributeGroup[@ref='ruleml:index-attrib.choice']">
    <xs:attributeGroup ref="ruleml:index.attrib.def"/>
  </xsl:template>
  

  <!-- Remove groups like 
  <xs:group name="_1">-->
  <xsl:template match="xs:group[@name='_1']"> </xsl:template>
  <xsl:template match="xs:group[@name='_2']"> </xsl:template>

  <xsl:template match="xs:attribute[@name='id']">
    <xs:attribute name="id" type="xs:ID"/>
  </xsl:template>

  <!-- Remove unused groups and elements -->
  <xsl:template match="xs:group[matches(@name,'^Dummy')]"></xsl:template>  
  <xsl:template match="xs:element[matches(@name,'^Dummy')]"></xsl:template>

  <!-- Remove existing includes -->
  <xsl:template match="xs:include"/>


  <!-- Remove unused Node.choice and edge.choice -->
  <xsl:template match="xs:group[@name='Node.choice']"/>
  <xsl:template match="xs:group[@name='edge.choice']"/>
  <xsl:template match="xs:group[@ref='ruleml:Node.choice']"/>
  <xsl:template match="xs:group[@ref='ruleml:edge.choice']"/>

  <!--Remove additional unneeded required attribute definitions -->
  <xsl:template match="xs:attributeGroup[@name='hasCreationDate.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='iri.lrml.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='key.lrml.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='keyref.lrml.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='memberType.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='over.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='pre.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='refersTo.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='refID.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='refIDSystemName.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='refIDSystemSource.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='refType.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='sameAs.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='strength.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='type.lrml.attrib.def']"/>
  <xsl:template match="xs:attributeGroup[@name='under.attrib.def']"/>    
  
  <!-- FIXME Repair attributes in xml namespace -->
  <xsl:template match="xs:attributeGroup[@ref='xml:xml-base.choice']">
    <xs:attribute ref="xml:base"/>
  </xsl:template>
  <xsl:template match="xs:attributeGroup[@ref='xml:xml-id.choice']">
    <xs:attribute ref="xml:id"/>
  </xsl:template>
          
    
<!-- Change attribute groups of leaf edges to complex types -->
  <xsl:template match="xs:attributeGroup[matches(@name,'type\.def$')]">
    <xsl:element name="xs:complexType">
      <xsl:apply-templates select="@*|node()"/>
    </xsl:element>
  </xsl:template>
  <xsl:template match="xs:element[xs:complexType[xs:attributeGroup[matches(@ref,'type\.def$')]]]">
    <xsl:variable name="type" select="./xs:complexType/xs:attributeGroup/@ref" as="xs:string"/>
    <xsl:element name="xs:element">
      <xsl:apply-templates select="@*"/>
      <xsl:attribute name="type" select="$type"/>
      <xsl:apply-templates select="*[name(.)!='xs:complexType']"/>      
    </xsl:element>
  </xsl:template>

    
<!-- Copies everything except XML comments to the transformation output -->
  <xsl:template match="@*|*|text()|processing-instruction()">
    <xsl:copy>
      <xsl:apply-templates select="@*|*|text()|processing-instruction()"/>
    </xsl:copy>
  </xsl:template>
  
</xsl:stylesheet>
