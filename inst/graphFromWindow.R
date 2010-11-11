library (RCytoscape)
library (XMLRPC)
library (RUnit)
#------------------------------------------------------------------------------------------------------------------------
test = function ()
{
  test.find.window.id ()
  test.haveNodeAttribute  ()
  test.haveEdgeAttribute  ()
  test.addNodeAttributesFromWindow ()
  test.addEdgeAttributesFromWindow ()
  test.getGraphFromWindow  ()

} # test
#------------------------------------------------------------------------------------------------------------------------
find.window.id = function (cyCon, window.title)
{
  current.window.list = getWindowList (cyCon)
  if (!window.title %in% as.character (current.window.list)) {
    write (sprintf ("No existing Cytoscape window named '%s'", window.title), stderr ())
    return (NA)
    } # 

  window.entry = which (as.character (current.window.list) == window.title)
  window.id =  as.character (names (current.window.list) [window.entry])

  return (window.id)

} # find.window.id
#------------------------------------------------------------------------------------------------------------------------
test.find.window.id = function ()
{
  print ('test.find.window.id')
  if (!exists ('cw3')) {
    cw3 <<- CytoscapeWindow ('cw3', graph=makeSimpleGraph ())
    displayGraph (cw3)
    redraw (cw3)
    layout (cw3)
    }

  if (!exists ('cyCon'))
     cyCon <<- CytoscapeConnection ()

  id = find.window.id (cyCon, 'cw3')
  checkEquals (id, cw3@window.id)

} # test.find.window.id
#------------------------------------------------------------------------------------------------------------------------
cy.noa.names = function (cwx)
{
  node.attribute.names <<- xml.rpc (cw@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
  result = c ()
  for (noa.name in node.attribute.names) {
     attribute.type = xml.rpc (cw@uri, 'Cytoscape.getNodeAttributeType', noa.name, .convert=T)
     result [noa.name] = attribute.type
     }

  return (result)

} # cy.noa.names
#------------------------------------------------------------------------------------------------------------------------
cy.eda.names = function (cwx)
{
  edge.attribute.names <<- xml.rpc (cwx@uri, 'Cytoscape.getEdgeAttributeNames', .convert=T)
  result = c ()
  for (eda.name in edge.attribute.names) {
     attribute.type = xml.rpc (cwx@uri, 'Cytoscape.getEdgeAttributeType', eda.name, .convert=T)
     result [eda.name] = attribute.type
     }

  return (result)

} # cy.eda.names
#------------------------------------------------------------------------------------------------------------------------
getNodeNames = function (cyCon, window.id)
{
  all.node.names <<- xml.rpc (cyCon@uri, "Cytoscape.getNodes", window.id)
  return (all.node.names)

} # cy.node.names
#------------------------------------------------------------------------------------------------------------------------
cy.edge.names = function (cwx)
{
  all.edge.names <<- xml.rpc (cwx@uri, "Cytoscape.getEdges", as.character (cwx@window.id))
  return (all.edge.names)

} # cy.edge.names
#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network
haveNodeAttribute = function (cyCon, node.names, attribute.name)
{
  indices.of.nodes.with.attribute.value = which (xml.rpc (cyCon@uri, 'Cytoscape.nodesHaveAttribute', attribute.name, node.names))

  if (length (indices.of.nodes.with.attribute.value) > 0)
    return (node.names [indices.of.nodes.with.attribute.value])
  else 
    return (character(0))

} # haveNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.haveNodeAttribute = function ()
{
  if (!exists ('cw3')) {
    cw3 <<- CytoscapeWindow ('cw3', graph=makeSimpleGraph ())
    displayGraph (cw3)
    redraw (cw3)
    layout (cw3)
    }

  if (!exists ('cyCon'))
     cyCon <<- CytoscapeConnection ()

  nodes.with.attribute <<- haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'lfc')
  checkEquals (sort (nodes.with.attribute),  c ('A', 'B', 'C'))

  checkEquals (length (haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'type')), 3)
  checkEquals (length (haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'label')), 3)
  checkEquals (length (haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'count')), 3)

  checkEquals (length (haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'bogus')), 0)


} # test.haveNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network
haveEdgeAttribute = function (cyCon, edge.names, attribute.name)
{
 #printf ('haveEdgeAttribute: %s', attribute.name)
 #printf ('            edges: %s', list.to.string (edge.names, sep='|'))
  indices.of.edges.with.attribute.value = which (xml.rpc (cyCon@uri, 'Cytoscape.edgesHaveAttribute', attribute.name, edge.names))
 #printf ('          indices: %s', list.to.string (indices.of.edges.with.attribute.value))

  if (length (indices.of.edges.with.attribute.value) > 0)
    return (edge.names [indices.of.edges.with.attribute.value])
  else 
    return (character(0))

} # haveEdgeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.haveEdgeAttribute = function ()
{
  if (!exists ('cw3')) {
    cw3 <<- CytoscapeWindow ('cw3', graph=makeSimpleGraph ())
    displayGraph (cw3)
    redraw (cw3)
    layout (cw3)
    }

  if (!exists ('cyCon'))
     cyCon <<- CytoscapeConnection ()

  cy2.edgenames = as.character (cy2.edge.names (getGraph (cw3)))
  edges.with.attribute <<- haveEdgeAttribute (cyCon, cy2.edgenames, 'edgeType')

  checkEquals (length (edges.with.attribute), 3)
  checkTrue ("A (phosphorylates) B" %in% edges.with.attribute)
  checkTrue ("B (synthetic lethal) C" %in% edges.with.attribute)
  checkTrue ("C (undefined) A" %in% edges.with.attribute)

  checkTrue (length (haveEdgeAttribute (cyCon, cy2.edgenames, 'score')) == 3)
  checkTrue (length (haveEdgeAttribute (cyCon, cy2.edgenames, 'misc')) == 3)
  checkTrue (length (haveEdgeAttribute (cyCon, cy2.edgenames, 'bogus')) == 0)

} # test.haveEdgeAttribute
#------------------------------------------------------------------------------------------------------------------------
addNodeAttributesFromWindow = function (cyCon, window.id, graph)
{
  node.attribute.names <<- xml.rpc (cyCon@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
  for (attribute.name in node.attribute.names) {
    known.node.names = getNodeNames (cyCon, window.id)
    nodes.with.attribute <<- haveNodeAttribute (cyCon, known.node.names, attribute.name)
    if (length (nodes.with.attribute) > 0) {
      write (sprintf ('retrieving "%s" attribute for %d nodes', attribute.name, length (nodes.with.attribute)), stderr ())
      attribute.type = xml.rpc (cyCon@uri, 'Cytoscape.getNodeAttributeType', attribute.name, .convert=T)
      if (attribute.type == 'INTEGER') {
        attribute.type = 'integer'
        default.value = 0
        }
      else if (attribute.type == 'STRING') {
        attribute.type = 'char'
        default.value = 'unassigned'
        }
      else if (attribute.type == 'FLOATING') {
        attribute.type = 'numeric'
        default.value = as.numeric (0.0)
        }
      graph = initNodeAttribute (graph, attribute.name, attribute.type, default.value)
      attribute.values <<- xml.rpc (cyCon@uri, 'Cytoscape.getNodesAttributes', attribute.name, nodes.with.attribute)
      nodeData (graph, nodes.with.attribute, attribute.name) = attribute.values
       } # if
    } # for

   return (graph)

} # addNodeAttributesFromWindow
#------------------------------------------------------------------------------------------------------------------------
test.addNodeAttributesFromWindow = function ()
{
  print ('test.getNodeAttributesFromWindow')

  if (!exists ('cyCon'))
     cyCon <<- CytoscapeConnection ()

  if (!exists ('cw3')) {
    cw3 <<- CytoscapeWindow ('cw3', graph=makeSimpleGraph ())
    displayGraph (cw3)
    redraw (cw3)
    layout (cw3)
    }

  g <<- new ('graphNEL', edgemode='directed')
  g <<- graph::addNode (c ('A', 'B', 'C'), g)
 #print (g)
  g2 <<- addNodeAttributesFromWindow (cyCon, find.window.id (cyCon, 'cw3'), g)

} # test.addNodeAttributesFromWindow
#------------------------------------------------------------------------------------------------------------------------
addEdgeAttributesFromWindow = function (cyCon, window.id, existing.graph)
{
  edge.attribute.names <<- xml.rpc (cyCon@uri, 'Cytoscape.getEdgeAttributeNames', .convert=T)
  write (sprintf ('creating %d cytoscape-style edge names', length (edgeNames (existing.graph))), stderr ())
  cy2.edgenames = as.character (cy2.edge.names (existing.graph))   # < 2 seconds for > 9000 edges

  for (attribute.name in edge.attribute.names) {
    edges.with.attribute <<- haveEdgeAttribute (cyCon, cy2.edgenames, attribute.name)
    if (length (edges.with.attribute) > 0) {
       write (sprintf ('retrieving "%s" attribute for %d edges', attribute.name, length (edges.with.attribute)), stderr ())
       attribute.type = xml.rpc (cyCon@uri, 'Cytoscape.getEdgeAttributeType', attribute.name, .convert=T)
       if (attribute.type == 'INTEGER') {
         attribute.type = 'integer'
         default.value = 0
         }
       else if (attribute.type == 'STRING') {
         attribute.type = 'char'
         default.value = 'unassigned'
         }
       else if (attribute.type == 'FLOATING') {
         attribute.type = 'numeric'
         default.value = as.numeric (0.0)
         }
       existing.graph = initEdgeAttribute (existing.graph, attribute.name, attribute.type, default.value)
       eda.value <<- xml.rpc (cyCon@uri, 'Cytoscape.getEdgesAttributes', attribute.name, edges.with.attribute)
       regex <<- ' *[\\(|\\)] *'
       edges.tokens <<- strsplit (edges.with.attribute, regex)
       source.nodes <<- unlist (lapply (edges.tokens, function (tokens) tokens [1]))
       target.nodes <<- unlist (lapply (edges.tokens, function (tokens) tokens [3]))
       edge.types <<-   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
       edgeData (existing.graph, source.nodes, target.nodes, attribute.name) = eda.value
       } # if
    } # for

   return (existing.graph)

} # addEdgeAttributesFromWindow
#------------------------------------------------------------------------------------------------------------------------
test.addEdgeAttributesFromWindow = function ()
{
  print ('test.getEdgeAttributesFromWindow')

  if (!exists ('cyCon'))
     cyCon <<- CytoscapeConnection ()

  if (!exists ('cw3')) {
    cw3 <<- CytoscapeWindow ('cw3', graph=makeSimpleGraph ())
    displayGraph (cw3)
    redraw (cw3)
    layout (cw3)
    }

  g = new ('graphNEL', edgemode='directed')
  g = graph::addNode (c ('A', 'B', 'C'), g)
  g = graph::addEdge("A", "B", g)
  g = graph::addEdge("B", "C", g)
  g = graph::addEdge("C", "A", g)
   # "C (undefined) A" "B (synthetic lethal) C"   "A (phosphorylates) B" 
  edgeDataDefaults (g, 'edgeType') = 'undefined'
  edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
  edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'
  edgeData (g, 'C', 'A', 'edgeType') = 'undefined'

  g2 <<- addEdgeAttributesFromWindow (cyCon, cw3, g)

  checkEquals (eda (g2, 'score') [['A|B']], 35)
  checkEquals (eda (g2, 'score') [['B|C']], -12)
  checkEquals (eda (g2, 'score') [['C|A']], 0)

  checkEquals (eda (g2, 'edgeType') [['A|B']], 'phosphorylates')
  checkEquals (eda (g2, 'edgeType') [['B|C']], 'synthetic lethal')
  checkEquals (eda (g2, 'edgeType') [['C|A']], 'undefined')

  checkEquals (eda (g2, 'interaction') [['A|B']], 'phosphorylates')
  checkEquals (eda (g2, 'interaction') [['B|C']], 'synthetic lethal')
  checkEquals (eda (g2, 'interaction') [['C|A']], 'undefined')

  checkEquals (eda (g2, 'misc') [['A|B']], 'default misc')
  checkEquals (eda (g2, 'misc') [['B|C']], 'default misc')
  checkEquals (eda (g2, 'misc') [['C|A']], 'default misc')

  checkEquals (eda (g2, 'canonicalName') [['A|B']],  "A (phosphorylates) B")
  checkEquals (eda (g2, 'canonicalName') [['B|C']],  "B (synthetic lethal) C")
  checkEquals (eda (g2, 'canonicalName') [['C|A']],  "C (undefined) A")


} # test.addEdgeAttributesFromWindow
#------------------------------------------------------------------------------------------------------------------------
getGraphFromWindow = function (window.title)
{
  cyCon = CytoscapeConnection ()
  window.id = find.window.id (cyCon, window.title)
  stopifnot (!is.na (window.id))

  all.node.names = xml.rpc (cyCon@uri, "Cytoscape.getNodes", window.id)
  write (sprintf ('recevied %d nodes from %s', length (all.node.names), window.title), stderr ())
  g = new ("graphNEL", edgemode='directed')
  write (sprintf ('adding %d nodes to local graph', length (all.node.names)), stderr ())
  g = graph::addNode (all.node.names, g)
  
  node.attribute.names = xml.rpc (cyCon@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
  g = initEdgeAttribute (g, 'edgeType', 'char', 'assoc')

  regex = ' *[\\(|\\)] *'
  all.edge.names = xml.rpc (cyCon@uri, "Cytoscape.getEdges", window.id)
  write (sprintf ('recevied %d edges from %s', length (all.edge.names), window.title), stderr ())
  edges.tokens = strsplit (all.edge.names, regex)

  source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
  target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
  edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
  write (sprintf ('adding %d edges to local graph', length (edges.tokens)), stderr ())
  g = addEdge (source.nodes, target.nodes, g)
  edgeData (g, source.nodes, target.nodes, 'edgeType') = edge.types
  g = addNodeAttributesFromWindow (cyCon, window.id, g)
  g = addEdgeAttributesFromWindow (cyCon, window.id, g)

  return (g)

} # getGraphFromWindow
#------------------------------------------------------------------------------------------------------------------------
test.getGraphFromWindow = function ()
{
  if (!exists ('cyCon'))
     cyCon <<- CytoscapeConnection ()

  if (!exists ('cw3')) {
    cw3 <<- CytoscapeWindow ('cw3', graph=makeSimpleGraph ())
    displayGraph (cw3)
    redraw (cw3)
    layout (cw3)
    } # create cw3

  g3 <<- getGraphFromWindow ('cw3')
  checkEquals (sort (nodes (g3)), c ('A', 'B', 'C'))
  checkEquals (sort (noa.names (g3)), c ("canonicalName", "count", "label", "lfc", "type"))
  checkEquals (as.character (sort (noa (g3, 'canonicalName'))), c ('A', 'B', 'C'))
  checkEquals (as.integer   (sort (noa (g3, 'count'))),         c (2, 30, 100))
  checkEquals (as.character (sort (noa (g3, 'label'))),         c ('Gene A', 'Gene B', 'Gene C'))
  checkEquals (as.numeric (sort (noa (g3, 'lfc'))),             c (-3,  0,  3))
  checkEquals (as.character (sort (noa (g3, 'type'))),          c ("glycoprotein", "kinase", "transcription factor"))

  checkEquals (sort (eda.names (g3)), c ("canonicalName", "edgeType", "interaction", "misc", "score"))

  checkEquals (sort (names (cy2.edge.names (g3))),        c ('A~B',                   'B~C',                    'C~A'))
  checkEquals (sort (as.character (cy2.edge.names (g3))), c ("A (phosphorylates) B",  "B (synthetic lethal) C", "C (undefined) A"))

  checkEquals (as.character (sort (eda (g3, 'edgeType'))), c ("phosphorylates", "synthetic lethal", "undefined"))
  checkEquals (as.character (sort (eda (g3, 'canonicalName'))), c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (undefined) A"))
  checkEquals (as.character (sort (eda (g3, 'interaction'))), c ("phosphorylates", "synthetic lethal", "undefined"))
  checkEquals (as.character (sort (eda (g3, 'misc'))), c ("default misc", "default misc", "default misc"))
  checkEquals (as.numeric (sort (eda (g3, 'score'))), c (-12,  0,  35))

} # test.getGraphFromWindow
#------------------------------------------------------------------------------------------------------------------------
