This hack is really just a programming excercise :-)

What data: The List event 2012

What I've done with it
---
- I've written a Haskell program that...
- Transforms the XML into RDF
- Defines a schema for The List specific fields
- Reuses ontologies for field e.g. location (wg84pos)
- Serializes the RDF into the Turtle file format

In addition...
- The program goes out to to the The List web API to grab more info
about locations
- Goes out to Linked Geo Data and discovers the nearby pubs to the
venue (within 100 metres)


Why?! have I done it?
---
- Linked Open Data is *really* awesome.
- It'd great to have The List on the cloud of linked open data
- ... on this map: http://richard.cyganiak.de/2007/10/lod/
- (More exposure on the semantic web for The List events!!)
- Venue location info is disambiguated, so all kinds of interesting
information can be sought from the semantic web (finding pubs is just
a proof-of-concept).

Files
---
- list_mappings.hs <- the Haskell code to generate the turtle file
- list.ttl <- the file generated
- README <- this file


What next?
--
- You can install a SPARQL endpoint... point it at the .ttl file, and
you have a *semantic search endpoint* for free out-of-the-box.

```
<#fringe>    rdf:type ja:RDFDataset ;
    rdfs:label "FringeData" ;
    ja:defaultGraph
      [ rdfs:label "" ;
        a ja:MemoryModel ;
        ja:content [ja:externalContent <file:/location/to/rdf_output.ttl> ] ;
      ] ;
    .
```

- Get in touch with me.. I'd happily add more (robstewart57@gmail.com).
