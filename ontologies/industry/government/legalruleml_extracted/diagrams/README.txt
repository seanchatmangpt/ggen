The diagrams in this directory are created from the RDFS files in the parallel
/modules directory using the online RDF Gravity
  http://semweb.salzburgresearch.at/apps/rdf-gravity/download.html
For each RDFS module, we present one or more graphs having one of 
the following "views":

1. The metamodel concept (rdfs:Class) tree shows the rdfs:subClass hierarchy.
All symbols in this graph are concept nodes (rdfs:Class), with color-coding to
indicate namespace
  blue: legalruleml
  green: xsd schema part 2 datatypes
  orange: ruleml
(e.g. upper-subclass.png)

2. The bipartite graph shows the rdfs:domain and rdfs:range properties for
the metamodel relationships (rdf:Property). The nodes of this graph alternate
between concept (rectangle) and relationship (triangle). 

This diagram is similar to an entity-relationship model (ERM) although the
conventional symbol for a relationship in ERM is a diamond rather than a triangle.

RDFS does not distinguish between object and literal properties, however the
properties whose range is colored green are data properties, as a green
rectangle indicates an XSD datatype.
(e.g. upper-properties.png)

3. A combined graph merges the concept tree and the bipartite graph.
    (e.g. context.png)