# RCytoscape/inst/test.R
#------------------------------------------------------------------------------------------------------------------------
library (RCytoscape)
#------------------------------------------------------------------------------------------------------------------------
run.tests = function ()
{
  options ('warn'=2)   # make sure that any R warnings are treated as fatal errors

    # before doing anything else, make sure that the Cytoscape plugin version is one we can respond to
  test.version ()
  
    # start with a clean slate, and no windows

  cy = CytoscapeConnection ()
  destroyAllWindows (cy)

  test.noa ()
  test.eda ()
  test.create.class ()
  test.destroyWindow ()
  test.destroyWindowByName ()
  test.destroyAllWindows ()
  test.getWindowID ()
  test.getWindowList ()
  test.getNodeShapes ()
  test.getAttributeClassNames ()
  test.getArrowShapes ()
  test.getLineStyles ()
  test.getLayoutNames ()
  test.sendNodes ()
  test.sendEdges ()
  test.sendNodeAttributes ()
  test.sendEdgeAttributes ()
  test.cy2.edge.names ()
  test.panelOperations ()

  test.setDefaultNodeShape ()
  test.setDefaultNodeColor ()
  test.setDefaultNodeSize ()
  test.setDefaultNodeBorderColor ()
  test.setDefaultNodeBorderWidth ()
  test.setDefaultNodeFontSize ()
  test.setDefaultNodeLabelColor ()
  test.setDefaultEdgeLineWidth ()
  test.setDefaultEdgeColor ()

  test.addGetAndDeleteNodeAttributes ()
  test.addGetAndDeleteEdgeAttributes ()

  test.setNodeLabelRule ()
  test.setEdgeLabelRule ()
  test.setNodeTooltipRule ()
  test.setEdgeTooltipRule ()
  test.setNodeColorRule ()
  test.setNodeBorderColorRule ()
  test.setNodeBorderWidthRule ()
  test.setNodeSizeRule ()
  test.setNodeShapeRule ()
  test.countNodes ()
  test.countEdges ()
  test.countNodesAndEdgesInEmptyGraph ()
  test.getAllNodes ()
  test.getAllEdges ()
  test.selectNodes ()
  test.selectEdges ()
  test.hideNodes ()
  #test.hideEdges ()
  
  test.setEdgeLineStyleRule ()
  test.setEdgeLineWidthRule ()
  test.setEdgeColorRule ()
  test.setEdgeTargetArrowRule ()
  test.setEdgeArrowColorRules ()
  test.setEdgeSourceArrowRule ()
  test.movie ()
  test.unmatchedAttributesError ()
  test.remove.redundancies.in.undirected.graph ()
  test.randomUndirectedGraph ()
  test.simpleGraph ()
  test.setGraph ()
  test.setPosition ()

  test.haveNodeAttribute ()
  test.haveEdgeAttribute ()
  test.copyNodeAttributesFromCyGraph ()
  test.copyEdgeAttributesFromCyGraph ()
  test.getGraphFromCyWindow ()
  test.addGraphToGraph ()

  test.getVisualStyleNames ()
  test.copyVisualStyle ()
  test.setVisualStyle ()

  test.defaultColors ()

  options ('warn'=0)

} # run.tests
#------------------------------------------------------------------------------------------------------------------------
test.version = function ()
{
  write ('test.version', stderr ())
  cy = CytoscapeConnection ()
  #version.string = version (cy)

  plugin.version.string = version (cy)
  string.tmp1 = strsplit (plugin.version.string,' ')[[1]][1]
  string.tmp2 = gsub ('[a-z]', '', string.tmp1)
  string.tmp3 = gsub ('[A-Z]', '', string.tmp2)
  major.minor.version = as.numeric (string.tmp3)


  #tokens = strsplit (version.string, ' ')[[1]][1]
  #version.numbers = as.integer (strsplit (tokens, '\\.')[[1]])
  #major.minor.version = version.numbers [1] + (version.numbers [2]/10.0)
  msg (cy, paste ('CytoscapeRPC version', major.minor.version))
  checkTrue (major.minor.version >= 1.3)

} # test.version
#------------------------------------------------------------------------------------------------------------------------
test.create.class = function ()
{
  write ('test.create.class', stderr ())
  g = new ('graphNEL')
  cw = new.CytoscapeWindow ('test.create.class', g)
  checkTrue (validObject (cw))

} # test.create.class
#------------------------------------------------------------------------------------------------------------------------
test.destroyWindow = function ()
{
  write ('test.destroyWindow', stderr ())
  cw = new.CytoscapeWindow ('test.destroyWindow', new ('graphNEL'))
  cy = CytoscapeConnection ()
  original.window.count = getWindowCount (cy)
  destroyWindow (cy, 'test.destroyWindow')
  msg (cw, 'destroyed one window')
  new.window.count = getWindowCount (cy)
  checkTrue (new.window.count == original.window.count - 1)

} # test.destroyWindow
#------------------------------------------------------------------------------------------------------------------------
test.destroyWindowByName = function ()
{
  write ('test.destroyWindowByName', stderr ())
  cw = new.CytoscapeWindow ('test.destroyWindowByName', new ('graphNEL'))
  original.window.count = getWindowCount (cw)
  cy = CytoscapeConnection ()
  destroyWindow (cy, 'test.destroyWindowByName')
  msg (cy, 'destroyed one window')
  new.window.count = getWindowCount (cy)
  checkTrue (new.window.count == original.window.count - 1)

} # test.destroyWindow
#------------------------------------------------------------------------------------------------------------------------
test.destroyAllWindows = function ()
{
  write ('test.destroyAllWindows', stderr ())
  cy = CytoscapeConnection ()
  destroyAllWindows (cy)
  new.window.count = getWindowCount (cy)
  checkEquals (new.window.count, 0)
  msg (cy, 'destroyed all windows')

} # test.destroyAllWindows
#------------------------------------------------------------------------------------------------------------------------
test.getWindowID = function ()
{
  print ('test.getWindowID')

  cw3 =  new.CytoscapeWindow ('test.getWindowID', graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

  cyCon = CytoscapeConnection ()

  id = getWindowID (cyCon, 'test.getWindowID')
  checkEquals (id, cw3@window.id)

  invisible (cw3)

} # test.getWindowID
#------------------------------------------------------------------------------------------------------------------------
test.getWindowList = function ()
{
  write ('test.getWindowList', stderr ())
  test.window.name = 'test.getWindowList'
  cw2 = new.CytoscapeWindow (test.window.name, new ('graphNEL'))
  window.list = getWindowList (cw2)
  checkTrue (test.window.name %in% as.character (getWindowList (cw2)))

  invisible (cw2)

} # test.getWindowList
#------------------------------------------------------------------------------------------------------------------------
test.getNodeShapes = function ()
{
  write ('test.getNodeShapes', stderr ())

  cy = CytoscapeConnection ()
  shapes = getNodeShapes (cy)
  checkTrue (length (shapes) > 10)
  msg (cy, 'getNodeShapes')

   # pick a few specific shapes to test
 checkTrue (all (sapply (c ('trapezoid', 'ellipse', 'triangle'), function (s) s %in% shapes)))

} # test.getNodeShapes
#------------------------------------------------------------------------------------------------------------------------
test.getAttributeClassNames = function ()
{
  write ('test.getAttributeClassNames', stderr ())

  cy = CytoscapeConnection ()
  possible.values = getAttributeClassNames (cy)
  checkTrue (grep ('numeric', possible.values) > 0)
  checkTrue (grep ('integer', possible.values) > 0)
  checkTrue (grep ('character', possible.values) > 0)


} # test.getNodeShapes
#------------------------------------------------------------------------------------------------------------------------
test.getArrowShapes = function ()
{
  write ('test.getArrowShapes', stderr ())

  cy = CytoscapeConnection ()
  shapes = getArrowShapes (cy)
  checkTrue (length (shapes) >= 8)

   # pick a few specific shapes to test
  msg (cy, 'getArrowShapes')
  checkTrue (all (sapply (c ('Diamond', 'T', 'Circle'), function (s) s %in% shapes)))

} # test.getArrowShapes
#------------------------------------------------------------------------------------------------------------------------
test.getLineStyles = function ()
{
  write ('test.getLineStyles', stderr ())

  cy = CytoscapeConnection ()
  styles = getLineStyles (cy)
  checkTrue (length (styles) > 10)

   # pick a few specific styles to test
  msg (cy, 'getLineStyles')
  checkTrue (all (sapply (c ('SOLID', 'DOT', 'EQUAL_DASH'), function (s) s %in% styles)))

} # test.getLineStyles
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutNames = function ()
{
  write ('test.getLayoutNames', stderr ())

  cy = CytoscapeConnection ()
  names = getLayoutNames (cy)
  checkTrue (length (names) > 15)

   # pick a few specific styles to test
  msg (cy, 'getLayoutNames')
  checkTrue (all (sapply (c ('grid', 'jgraph-spring', 'circular'), function (s) s %in% names)))

} # test.getLayoutNames
#------------------------------------------------------------------------------------------------------------------------
test.sendNodes = function ()
{
  write ('test.sendNodes', stderr ())
  g = RCytoscape::makeSimpleGraph ()
  cwa = new.CytoscapeWindow ('test.sendNodes', graph=g)
  sendNodes (cwa)
  layout (cwa, "grid")   # no edges, so other layouts will simply superimpose the nodes
  redraw (cwa)
  msg (cwa, 'sendNodes')

  invisible (cwa)

} # test.sendNodes
#------------------------------------------------------------------------------------------------------------------------
test.sendEdges = function ()
{
  write ('test.sendEdges', stderr ())
  g = RCytoscape::makeSimpleGraph ()
  cwe = new.CytoscapeWindow ('test.sendEdges', graph=g)
  sendNodes (cwe)
  sendEdges (cwe)
  layout (cwe, 'jgraph-circle')
  redraw (cwe)
  msg (cwe, 'sendEdges')

  invisible (cwe)

} # test.sendEdges
#------------------------------------------------------------------------------------------------------------------------
test.sendNodeAttributes = function ()
{
  write ('test.sendNodeAttributes', stderr ())
  g = RCytoscape::makeSimpleGraph ()
  cwb = new.CytoscapeWindow ('test.sendNodeAttributes', graph=g)
  sendNodes (cwb)
  attribute.names = noa.names (g)

  for (attribute.name in attribute.names) {
    result = sendNodeAttributes (cwb, attribute.name)
    }

  layout (cwb, 'grid')
  redraw (cwb)
  msg (cwb, 'sendNodeAttributes')


    # now call the direct method, which -- in contrast to the unmarked method (sendNodeAttributes) -- does not
    # extract attributes from the graph; they are instead supplied separately, and thus are well suited to
    # successive updates, as in a movie

  result = sendNodeAttributesDirect (cwb, 'count', 'int', c ('A', 'B', 'C'), c (38, 105, 0))

    # sending a single attribute to CytoscapeRPC runs into a problem:  xmlrpc maps these to scalars,
    # rather than as lists of length 1, and no CytoscapeRPC method is matched.
    # (10 dec 2010) RCytoscape::sendNodeAttributesDirect solves this inelegantly, but duplicating
    # the node name and attribute values, making lists of length 2
    # 
  result = sendNodeAttributesDirect (cwb, 'count', 'int', 'A', 432)

  invisible (cwb)


} # test.sendNodeAttributes
#------------------------------------------------------------------------------------------------------------------------
# depends on prior creation of cwe by test.sendEdges
test.sendEdgeAttributes = function ()
{
  write ('test.sendEdgeAttributes', stderr ())
  cwe = new.CytoscapeWindow ('test.sendEdgeAttributes', graph=makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)
  attribute.names = eda.names (cwe@graph)

  for (attribute.name in attribute.names) {
    result = sendEdgeAttributes (cwe, attribute.name)
    } 

  edge.names = as.character (cy2.edge.names (cwe@graph))
  checkEquals (length (edge.names), 3)
  edge.values = c ('alligator', 'hedgehog', 'anteater')
  result = sendEdgeAttributesDirect (cwe, 'misc', 'string', edge.names, edge.values)

    # sending a single attribute to CytoscapeRPC runs into a problem:  xmlrpc maps these to scalars,
    # rather than as lists of length 1, and no CytoscapeRPC method is matched.
    # (10 dec 2010) RCytoscape::sendEdgeAttributesDirect solves this inelegantly, but duplicating
    # the edge name and attribute values, making lists of length 2

  result = sendEdgeAttributesDirect (cwe, 'misc', 'string', edge.names [1], edge.values [1])

  msg (cwe, 'sendEdgeAttributes')

  invisible (cwe)  

} # test.sendEdgeAttributes
#------------------------------------------------------------------------------------------------------------------------
test.noa = function ()
{
  write ('test.noa', stderr ())

  g.simple = makeSimpleGraph ()

  result = noa (g.simple, 'type')
  checkEquals (length (result), 3)
  checkEquals (sort (names (result)), c ('A', 'B', 'C'))
  checkEquals (result [['A']], 'kinase')
  checkEquals (result [['B']], 'transcription factor')
  checkEquals (result [['C']], 'glycoprotein')

  checkTrue (is.na (noa (g.simple, 'bogusAttributeName')))
  invisible (g.simple)

} # test.noa
#------------------------------------------------------------------------------------------------------------------------
test.eda = function ()
{
  write ('test.eda', stderr ())

  g.simple = makeSimpleGraph ()

  result = eda (g.simple, 'edgeType')
  checkEquals (length (result), 3)
  checkEquals (sort (names (result)), c ("A|B", "B|C", "C|A"))
  checkEquals (result [['A|B']], 'phosphorylates')
  checkEquals (result [['B|C']], 'synthetic lethal')
  checkEquals (result [['C|A']], 'undefined')

  checkTrue (is.na (eda (g.simple, 'bogusAttributeName')))
  invisible (g.simple)

} # test.eda
#------------------------------------------------------------------------------------------------------------------------
test.cy2.edge.names = function ()
{
  write ('test.cy2.edge.names', stderr ())
  g = RCytoscape::makeSimpleGraph ()

    # this graph has the expected 'edgeType' edge attribute, used to make a standard cytoscape edge name
  edge.names = cy2.edge.names (g)
  checkEquals (edge.names [['A~B']], "A (phosphorylates) B")
  checkEquals (edge.names [['B~C']], "B (synthetic lethal) C")
  checkEquals (edge.names [['C~A']], "C (undefined) A")

    # now create a tiny graph, two nodes, one edge, with NO edgeType attribute.  make sure it is converted properly
  g2 =  new ('graphNEL', edgemode='directed')

  g2 = graph::addNode ('A', g2)
  g2 = graph::addNode ('B', g2)
  g2 = graph::addEdge ('A', 'B', g2)

  edge.names.2 = cy2.edge.names (g2)  
  checkEquals (edge.names.2 [['A~B']], "A (unspecified) B")

  g3 = new ('graphNEL', edgemode='directed')
  edge.names.should.be.empty = cy2.edge.names (g3)
  checkTrue (is.na (edge.names.should.be.empty))

  invisible (g3)

} # test.cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
# depends on prior creation of cw by test.createClass, providing a new.CytoscapeWindow object, with a 'uri' slot
test.panelOperations = function ()
{
  cw = new.CytoscapeWindow ('test.panelOperations')

  hidePanel (cw, 'Control Panel')
  hidePanel (cw, 'd')

  floatPanel (cw, 'Control Pa')
  floatPanel (cw, 'DATA')

  dockPanel (cw, 'control ')
  dockPanel (cw, 'data panel')
  msg (cw, 'test.panelOperations')

} # test.panelOperations
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeShape = function (direct=FALSE)
{
  write ('test.setDefaultNodeShape', stderr ())

  cwe = new.CytoscapeWindow ('test.setDefaultNodeShape', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

   hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
   shapes = getNodeShapes (cwe)

   if (direct) {  # debug
     for (shape in shapes) {
       if (!exists ('xml.rpc')) library (XMLRPC)
       xml.rpc (cwe@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Shape', shape); 
       redraw (cwe); 
       system ('sleep 1')
       } # for shape
     } # direct

  setDefaultNodeShape (cwe, 'octagon'); redraw (cwe)
  msg (cwe, 'octagon')
  system ('sleep 1')
  setDefaultNodeShape (cwe, 'ellipse');  redraw (cwe)
  msg (cwe,'ellipse')
  system ('sleep 1')
  setDefaultNodeShape (cwe, 'triangle');  redraw (cwe)
  msg (cwe, 'triangle')

  invisible (cwe)

} # test.setDefaultNodeShape
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeColor = function (direct=FALSE)
{
  write ('test.setDefaultNodeColor', stderr ())

  cwe = new.CytoscapeWindow ('test.setDefaultNodeColor', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  if (direct) {  # useful for debuggin
    for (i in 1:3) {
      if (!exists ('xml.rpc')) library (XMLRPC)
      xml.rpc (cwe@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Color', '#AAAA00'); redraw (cwe)
      xml.rpc (cwe@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Color', '#00AAAA'); redraw (cwe)
      } # for i
    } # direct

  setDefaultNodeColor (cwe, '#AA00AA')
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeSize = function (direct=FALSE)
{
  write ('test.setDefaultNodeSize', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultNodeSize', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  for (i in 1:3) {
    setDefaultNodeSize (cwe, 20)
    redraw (cwe)
    system ('sleep 1')
    setDefaultNodeSize (cwe, 200)
    redraw (cwe)
    system ('sleep 1')
    } # for i

  setDefaultNodeSize (cwe, 60)
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeSize
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeBorderColor = function (direct=FALSE)
{
  write ('test.setDefaultNodeBorderColor', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultNodeBorderColor', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  for (i in 1:3) {
    setDefaultNodeBorderColor (cwe, '#FFFFFF'); 
    redraw (cwe)
    system ('sleep 1')
    setDefaultNodeBorderColor (cwe, '#FF0000'); 
    redraw (cwe)
    system ('sleep 1')
    } # for i

  invisible (cwe)

} # test.setDefaultNodeBorderColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeBorderWidth = function (direct=FALSE)
{
  write ('test.setDefaultNodeBorderWidth', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultNodeBorderWidth', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultNodeBorderWidth (cwe, 5)
    redraw (cwe)
    system ('sleep 1')
    setDefaultNodeBorderWidth (cwe, 0)
    redraw (cwe)
    system ('sleep 1')
    } # for i

  setDefaultNodeBorderWidth (cwe, 1)
  redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeBorderWidth
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeFontSize = function (direct=FALSE)
{
  write ('test.setDefaultNodeFontSize', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultNodeFontSize', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   
  for (i in 1:3) {
    setDefaultNodeFontSize (cwe, 3); redraw (cwe)
    system ('sleep 1')
    setDefaultNodeFontSize (cwe, 30); redraw (cwe)
    system ('sleep 1')
    }
  
  setDefaultNodeFontSize (cwe, 12); redraw (cwe)

  invisible (cwe)

} # test.setDefaultNodeFontSize
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultNodeLabelColor = function (direct=FALSE)
{
  write ('test.setDefaultNodeLabelColor', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultNodeLabelColor', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultNodeLabelColor (cwe, '#FFAAAA');redraw (cwe)
    system ('sleep 1')
    setDefaultNodeLabelColor (cwe, '#000000');redraw (cwe)
    system ('sleep 1')
    } # for i

  invisible (cwe)

} # test.setDefaultNodeLabelColor
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultEdgeLineWidth = function (direct=FALSE)
{
  write ('test.setDefaultEdgeLineWidth', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultEdgeLineWidth', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultEdgeLineWidth (cwe, 5); redraw (cwe)
    system ('sleep 1')
    setDefaultEdgeLineWidth (cwe, 0); redraw (cwe)
    system ('sleep 1')
    }

  setDefaultEdgeLineWidth (cwe, 1); redraw (cwe)

  invisible (cwe)

} # test.setDefaultEdgeLineWidth
#------------------------------------------------------------------------------------------------------------------------
test.setDefaultEdgeColor = function (direct=FALSE)
{
  write ('test.setDefaultEdgeColor', stderr ())
  cwe = new.CytoscapeWindow ('test.setDefaultEdgeColor', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');   

  for (i in 1:3) {
    setDefaultEdgeColor (cwe, '#FFFFFF'); redraw (cwe)
    system ('sleep 1')
    setDefaultEdgeColor (cwe, '#FF0000'); redraw (cwe)
    system ('sleep 1')
    } # for i

  setDefaultEdgeColor (cwe, '#000000'); redraw (cwe)

  invisible (cwe)

} # test.setDefaultEdgeColor
#------------------------------------------------------------------------------------------------------------------------
test.setNodeLabelRule = function ()
{
  write ('test.setNodeLabelRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setNodeLabelRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setNodeLabelRule (cwe, 'label')
  system ('sleep 1')
  setNodeLabelRule (cwe, 'type')
  system ('sleep 1')
  setNodeLabelRule (cwe, 'lfc')
  system ('sleep 1')
  setNodeLabelRule (cwe, 'count')
  system ('sleep 1')
  setNodeLabelRule (cwe, 'label')
  msg (cwe, 'test.setNodeLabelRule')

  invisible (cwe)

}  # test.setNodeLabelRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLabelRule = function ()
{
  write ('test.setEdgeLabelRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeLabelRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setEdgeLabelRule (cwe, 'edgeType')
  system ('sleep 1')
  setEdgeLabelRule (cwe, 'score')
  system ('sleep 1')
  setEdgeLabelRule (cwe, 'canonicalName')
  msg (cwe, 'test.setEdgeLabelRule')

  invisible (cwe)

}  # test.setEdgeLabelRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeTooltipRule = function ()
{
  write ('test.setNodeTooltipRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setNodeTooltipRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  #setNodeLabelRule (cwe, 'label')
  setNodeTooltipRule (cwe, 'type')
  #setNodeLabelRule (cwe, 'lfc')
  #setNodeLabelRule (cwe, 'count')
  #setNodeLabelRule (cwe, 'label')
  msg (cwe, 'test.setNodeTooltipRule')

  invisible (cwe)

}  # test.setNodeTooltipRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTooltipRule = function ()
{
  write ('test.setEdgeTooltipRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeTooltipRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setEdgeTooltipRule (cwe, 'edgeType')
  msg (cwe, 'test.setEdgeTooltipRule')

  invisible (cwe)

}  # test.setEdgeTooltipRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeColorRule = function ()
{
  cwe = new.CytoscapeWindow ('test.setNodeColorRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');
  write ('test.setNodeColorRule', stderr ())

    # first, specify a mode='interpolate' rule -- the default
  node.attribute.values = c (-3.0, 0.0, 3.0)
  node.colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
  setNodeColorRule (cwe, 'lfc', node.attribute.values, node.colors, mode='interpolate')
  system ('sleep 1')

    # now, a lookup rule
  node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
  node.colors =           c ('#8888FF', '#00F088',              "#00CCCC")
  setNodeColorRule (cwe, 'type', node.attribute.values, node.colors, mode='lookup')
  system ('sleep 1')

    # now, a lookup rule with an incomplete lookup table:  does the default.color argument work?  cy2.7 bug -- not yet.
    # instead, the node is painted the cytoscape default color, pale red
  node.attribute.values = c ("kinase",  "transcription factor")
  node.colors =           c ('#8888FF', '#00F088')
  setNodeColorRule (cwe, 'type', node.attribute.values, node.colors, mode='lookup', default.color='#AA33AA')
  msg (cwe, 'test.setNodeColorRule')

    # now, use 1 element lists.
  node.attribute.values = c ("kinase")
  node.colors =           c ('#FFFFFF')
  setNodeColorRule (cwe, 'type', node.attribute.values, node.colors, mode='lookup', default.color='#AA33AA')
  msg (cwe, 'test.setNodeColorRule')

  invisible (cwe)

} # test.setNodeColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeBorderColorRule = function ()
{
  cwe = new.CytoscapeWindow ('test.setNodeBorderColorRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');
  write ('test.setNodeBorderColorRule', stderr ())

    # set the stage by making all the nodes white, to provide better contrast for the node border colors
  node.attribute.values = c (-3.0, 0.0, 3.0)
  colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF')
  setNodeColorRule (cwe, 'lfc', node.attribute.values, colors, mode='interpolate')

    # first, specify a mode='interpolate' rule -- the default
  node.attribute.values = c (-3.0, 0.0, 3.0)
  colors = c ('#008800', '#00FF00', '#FFFFFF', '#FF0000', '#880000')
  setNodeBorderColorRule (cwe, 'lfc', node.attribute.values, colors, mode='interpolate')
  system ('sleep 1')

    # now, a lookup rule.  bright red, green and blue borders
  node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
  colors =                c ('#FF0000', '#00FF00',              "#0000FF")
  setNodeBorderColorRule (cwe, 'type', node.attribute.values, colors, mode='lookup')
  system ('sleep 1')

    # now, a lookup rule with an incomplete lookup table:  does the default.color argument work?  cy2.7 bug -- not yet.
    #  the glycoprotein node, 'Gene C', should have a white border around white fill
  node.attribute.values = c ("kinase",  "transcription factor")
  colors =                c ('#0000FF', '#FF0000')
  setNodeBorderColorRule (cwe, 'type', node.attribute.values, colors, mode='lookup', default.color='#FFFFFF')

    # now, one element lists
  node.attribute.values = c ("transcription factor")
  colors =                c ('#FF00FF')
  setNodeBorderColorRule (cwe, 'type', node.attribute.values, colors, mode='lookup', default.color='#FFFFFF')

  msg (cwe, 'test.setNodeBorderColorRule')
 
  invisible (cwe)

} # test.setNodeBorderColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeBorderWidthRule = function ()
{
  window.name = 'test.setNodeBorderWidthRule'
  cy = CytoscapeConnection ()
  if (window.name %in% as.character (getWindowList (cy)))
    destroyWindow (cy, window.name)

  cwe = new.CytoscapeWindow (window.name, graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hideAllPanels (cy)
  write ('test.setNodeBorderWidthRule', stderr ())

    # set the stage by making all the nodes white, to provide better contrast for the node border colors
  node.attribute.values = c (-3.0, 0.0, 3.0)
  colors = c ('#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF', '#FFFFFF')
  setNodeColorRule (cwe, 'lfc', node.attribute.values, colors, mode='interpolate')
  setDefaultNodeBorderColor (cwe, '#FF0000')

  for (i in 1:3) {
       # 3 different node border sizes
     node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
     border.widths =         c (0, 10, 20)
     setNodeBorderWidthRule (cwe, 'type', node.attribute.values, border.widths)
       # swap them around different node border sizes
     node.attribute.values = c ("kinase",  "transcription factor", "glycoprotein")
     border.widths =         c (20, 0, 10);
     setNodeBorderWidthRule (cwe, 'type', node.attribute.values, border.widths)
     } # for i   

  invisible (cwe)

} # test.setNodeBorderWidthRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeSizeRule = function ()
{
  write ('test.setNodeSizeRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setNodeSizeRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  hidePanel (cwe, 'd');   hidePanel (cwe, 'c');

    # first, create a simple 2-point rule, with 'below' and 'above' values strong enough to see that they are working
    # recall that makeSimpleGraph creates count attributes like this:
    # noa (getGraph (cwe), 'count')     #   A.A   B.B   C.C 
    #                                       "2"  "30" "100" 

  count.control.points = c (20,  40)
  node.sizes           = c (1, 80,  120, 300)
  setNodeSizeRule (cwe, 'count', count.control.points, node.sizes, mode='interpolate')
  system ('sleep 2')

    # now chop off the below & above values.  A should grow to 80, almost as big as B, and C should shrink to 120, larger that B

  count.control.points = c (20,  40)
  node.sizes           = c (80,  120)
  setNodeSizeRule (cwe, 'count', count.control.points, node.sizes, mode='interpolate')
  system ('sleep 2')

    # now use a mode='lookup' rule
  discrete.values = c ('Gene A', 'Gene B', 'Gene C')
  node.sizes = c (60, 10, 20)
  setNodeSizeRule (cwe, 'label', discrete.values, node.sizes, mode='lookup')
  setNodeSizeRule (cwe, 'label', discrete.values, 2 * (node.sizes), mode='lookup')

  msg (cwe, 'test.setNodeSizeRule')

  invisible (cwe)

} # test.setNodeSizeRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeShapeRule = function ()
{
  write ('test.setNodeShapeRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setNodeShapeRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

     # specify shapes for only two of the three nodes and node types.  make sure that the third node gets
     # the default shape
  
     # make rule for 2 of 3 node types, leaving the third as the default
  node.shapes = c ('diamond', 'triangle')
  attribute.values = c ('kinase', 'glycoprotein')
  setNodeShapeRule (cwe, node.attribute.name='type', attribute.values, node.shapes, default.shape='ellipse')

     # test one-element lists
  node.shapes = c ('diamond')
  attribute.values = c ('glycoprotein')
  setNodeShapeRule (cwe, node.attribute.name='type', attribute.values, node.shapes, default.shape='ellipse')

  msg (cwe, 'test.setNodeShapeRule')

  invisible (cwe)

} # test.setNodeShapeRule
#------------------------------------------------------------------------------------------------------------------------
test.countNodes = function ()
{
  write ('test.countNodes', stderr ())
  cwe = new.CytoscapeWindow ('test.countNodes', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)
  checkEquals (getNodeCount (cwe), length (nodes (getGraph (cwe))))

  invisible (cwe)

} # test.countNodes
#------------------------------------------------------------------------------------------------------------------------
test.countEdges = function ()
{
  write ('test.countEdges', stderr ())
  cwe = new.CytoscapeWindow ('test.countEdges', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)
  checkEquals (getEdgeCount (cwe), length (edgeNames (getGraph (cwe))))

  invisible (cwe)

} # test.countNodes
#------------------------------------------------------------------------------------------------------------------------
test.countNodesAndEdgesInEmptyGraph = function ()
{
  write ('test.countNodesAndEdgesInEmptyGraph', stderr ())
  g.empty = new ("graphNEL", edgemode = "directed")
  checkEquals (length (nodes (g.empty)), 0)
  checkEquals (length (edges (g.empty)), 0)

  cwe = new.CytoscapeWindow ('test.countNodesAndEdgesInEmptyGraph', graph=g.empty)  # default behavior, but let's make it explicit
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)
  checkEquals (getNodeCount (cwe), 0)
  checkEquals (getEdgeCount (cwe), 0)

  invisible (cwe)

} # test.countNodesAndEdgesInEmptyGraph 
#------------------------------------------------------------------------------------------------------------------------
test.getAllNodes = function ()
{
  write ('test.getAllNodes', stderr ())

  cwe = new.CytoscapeWindow ('test.getAllNodes', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  cwe.nodes = getAllNodes (cwe)
  checkEquals (length (intersect (cwe.nodes, nodes (cwe@graph))), 3)

  msg (cwe, 'test.getAllNodes')

  invisible (cwe)

} # test.getAllNodes
#------------------------------------------------------------------------------------------------------------------------
test.getAllEdges = function ()
{
  write ('test.getAllEdges', stderr ())

  cwe = new.CytoscapeWindow ('test.getAllEdges', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  cwe.edges = getAllEdges(cwe)
  checkTrue ("C (undefined) A" %in% cwe.edges)
  checkTrue ("B (synthetic lethal) C" %in% cwe.edges)
  checkTrue ("A (phosphorylates) B" %in% cwe.edges)

  msg (cwe, 'test.getAllEdges')

  invisible (cwe)

} # test.getAllEdges
#------------------------------------------------------------------------------------------------------------------------
test.selectNodes = function ()
{
  write ('test.selectNodes', stderr ())
  cwe = new.CytoscapeWindow ('test.selectNodes', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)
  clearSelection (cwe)
  msg (cwe, 'test.selectNodes')

  invisible (cwe)

} # test.selectNodes
#------------------------------------------------------------------------------------------------------------------------
test.invertSelection = function ()
{
  title = 'test.invertSelection'
  cyCon = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cyCon)))
     destroyWindow (cyCon, title)

  write (title, stderr ())
  cwe = new.CytoscapeWindow (title, graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)

  for (i in 1:5) {
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 1)
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 2)
    } # for i

  
  #removeSelectedNodes ()
  clearSelection (cwe)

  invisible (cwe)

} # test.invertSelection 
#------------------------------------------------------------------------------------------------------------------------
test.removeSelectedNodes = function ()
{
  title = 'test.removeSelectedNodes'
  cyCon = CytoscapeConnection ()
  if (title %in% as.character (getWindowList (cyCon)))
     destroyWindow (cyCon, title)

  write (title, stderr ())
  cwe = new.CytoscapeWindow (title, graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)

  for (i in 1:5) {
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 1)
    invertNodeSelection (cwe)
    redraw (cwe)
    checkEquals (getSelectedNodeCount (cwe), 2)
    } # for i

  invisible (cwe)

} # test.invertNodeSelection 
#------------------------------------------------------------------------------------------------------------------------
# reveals unexpected behavior of 'unhideAll':  nodes & edges from other unknown places are 'unhidden' as well (pshannon: 07 jan 2011)
test.hideNodes = function ()
{
  write ('test.hideNodes', stderr ())
  cwe = new.CytoscapeWindow ('test.hideNodes', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)
  checkEquals (getNodeCount (cwe), 3)
  hideSelectedNodes (cwe)
  checkEquals (getNodeCount (cwe), 1)
  unhideAll (cwe)
  layout (cwe)
  redraw (cwe)
  #checkEquals (getNodeCount (cwe), 3)
  msg (cwe, 'test.selectNodes')

  invisible (cwe)

} # test.hideNodes
#------------------------------------------------------------------------------------------------------------------------
test.selectEdges = function ()
{
  write ('test.selectEdges', stderr ())
  cwe = new.CytoscapeWindow ('test.selectEdges', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  clearSelection (cwe)
  checkEquals (getSelectedEdgeCount (cwe), 0)
    # not yet possible to select edges through CytoscapeRPCCallHandler
    # selectEdges (cwe, "A (phosphorylates) B")
    # checkEquals (getSelectedEdgeCount (cwe), 1)
  clearSelection (cwe)
  checkEquals (getSelectedEdgeCount (cwe), 0)

  msg (cwe, 'test.selectEdges')

  invisible (cwe)

} # test.selectEdges
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineStyleRule = function ()
{
  write ('test.setEdgeLineStyleRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeLineStyleRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)


  line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 3)

  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)

    # test one-element lists
  line.styles = c ('DOT')
  edgeType.values = c ('synthetic lethal')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 1)
  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)

  msg (cwe, 'test.setEdgeLineStyleRule')

  invisible (cwe)

} # test.setEdgeLineStyleRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineWidthRule = function ()
{
  write ('test.setEdgeLineWidthRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeLineWidthRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 3)

  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)
  setEdgeLineWidthRule (cwe, 'edgeType', edgeType.values, c (0, 8, 16))

    # try one-element lists
  setEdgeLineWidthRule (cwe, 'edgeType', edgeType.values [1], 10)
  
  msg (cwe, 'test.setEdgeLineStyleRule')

  invisible (cwe)

} # test.setEdgeLineWidthRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeColorRule = function ()
{
  write ('test.setEdgeColorRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeColorRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  colors = c ('#FF0000', '#FFFF00', '#00FF00')
  setEdgeColorRule (cwe, 'edgeType',  edgeType.values, colors)
  system ('sleep 1')

  all.white  = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
  setEdgeColorRule (cwe, 'edgeType',  edgeType.values [2], '#000000')

  msg (cwe, 'test.setEdgeColorRule')

  invisible (cwe)

} # test.setEdgeColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTargetArrowRule = function ()
{
  write ('test.setEdgeTargetArrowRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeTargetArrowRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  arrows = c ('Delta', 'T', 'Diamond')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 3)

  setEdgeTargetArrowRule (cwe, 'edgeType', edgeType.values, arrows)
  msg (cwe, 'test.setEdgeTargetArrowRule')

    # now test the list-of-length-one call.  the called method will double the list to get past the xmlrpc
    # treatment of lists of length one as scalars, and a failed signature match

  arrows = c ('Circle')
  edgeType.values = c ('phosphorylates')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 1)

  setEdgeTargetArrowRule (cwe, 'edgeType', edgeType.values, arrows)
  msg (cwe, 'test.setEdgeTargetArrowRule')

  invisible (cwe)

} # test.setEdgeTargetArrowRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeArrowColorRules = function ()
{
  write ('test.setEdgeArrowColorRules', stderr ())

  cwe = new.CytoscapeWindow ('test.setEdgeArrowColorRules', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  #xml.rpc (cwe@uri, 'Cytoscape.discreteMapper', as.character (cwe@window.id), 'default', 'edgeType', 'Edge Target Arrow Color',
  #                  '#FFFFFF', c ("phosphorylates", "synthetic lethal", "undefined"), 
  #                  #c ("#0000AA", "#00AA00", "#AA0000"))
  #                  c ("#AA00AA", "#AAAA00", "#AA0000"))

  #xml.rpc (cwe@uri, 'Cytoscape.createContinuousEdgeVisualStyle', 'edgeType', 'Edge Target Arrow Color',
  #         c (-40, 0, 40), c ('#00FF00', '#FFFFFF', '#FF0000'))

  colors.1 = c ("#FFFFFF", "#FFFFFF", "#FFFFFF")
  colors.2 = c ("#AA00AA", "#AAAA00", "#AA0000")

  setEdgeTargetArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.1)
  setEdgeSourceArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.1)
  system ('sleep 2')
  setEdgeTargetArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.2)
  setEdgeSourceArrowColorRule (cwe, 'edgeType', c ("phosphorylates", "synthetic lethal", "undefined"), colors.2)

    # test one-element list
  setEdgeSourceArrowColorRule (cwe, 'edgeType', "phosphorylates", '#000000')

  msg (cwe, 'test.setEdgeArrowColorRules')

  invisible (cwe)

} # test.setEdgetArrowColorRules
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowRule = function ()
{
  write ('test.setEdgeSourceArrowRule', stderr ())

  cwe = new.CytoscapeWindow ('test.setSourceArrowRule', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  arrows = c ('Arrow', 'Diamond', 'Circle')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 3)

  setEdgeSourceArrowRule (cwe, 'edgeType', edgeType.values, arrows)

   # test one-element rule
  setEdgeSourceArrowRule (cwe, 'edgeType', edgeType.values [2], arrows [2])

  msg (cwe, 'test.setEdgeSourceArrowRule')

  invisible (cwe)

} # test.setEdgeSourceArrowRule
#------------------------------------------------------------------------------------------------------------------------
test.movie = function ()
{
  write ('test.movie', stderr ())

  cwe = new.CytoscapeWindow ('test.movie', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cwe, 'count', count.control.points, sizes)
  setNodeColorRule (cwe, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'))

  count = 3

  for (i in 1:count) { 
    nodeData (cwe@graph, 'A', 'lfc') = -3.0
    nodeData (cwe@graph, 'B', 'lfc') = -0.7
    nodeData (cwe@graph, 'C', 'lfc') = -1.9
    nodeData (cwe@graph, 'A', 'count') = 10
    nodeData (cwe@graph, 'B', 'count') = 140
    nodeData (cwe@graph, 'C', 'count') = 32
    result = sendNodeAttributes (cwe, 'lfc')
    result = sendNodeAttributes (cwe, 'count')
    redraw (cwe)

    system ('sleep 1')
    nodeData (cwe@graph, 'A', 'lfc') = 3.0
    nodeData (cwe@graph, 'B', 'lfc') = 0.7
    nodeData (cwe@graph, 'C', 'lfc') = 1.9
    nodeData (cwe@graph, 'A', 'count') = 50
    nodeData (cwe@graph, 'B', 'count') = 22
    nodeData (cwe@graph, 'C', 'count') = 180
    result = sendNodeAttributes (cwe, 'lfc')
    result = sendNodeAttributes (cwe, 'count')
    redraw (cwe)
    system ('sleep 1')

    count.A = round (runif (1, 1, 200))
    count.B = round (runif (1, 1, 200))
    count.C = round (runif (1, 1, 200))

    result = sendNodeAttributesDirect (cwe, 'count', 'int', c ('A', 'B', 'C'), c (count.A, count.B, count.C)); 
    result = sendNodeAttributesDirect (cwe, 'lfc', 'numeric', c ('A', 'B', 'C'), c (-1.0, 0.0, 1.0))
    redraw (cwe)

    if (i < count) system ('sleep 1')
    } # for i

  msg (cwe, 'test.movie')

  invisible (cwe)

} # test.movie
#------------------------------------------------------------------------------------------------------------------------
test.unmatchedAttributesError = function ()
{
  print ('test.unmatchedAttributesError')
  cwe = new.CytoscapeWindow ('unmatched attributes error', RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

    # this works
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cwe, 'count', count.control.points, sizes)
  redraw (cwe)

    # this should fail gracefully
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cwe, 'count', count.control.points, sizes)

  redraw (cwe)
  msg (cwe, 'test.unmatchedAttributesError')

  invisible (cwe)

} # test.unmatchedAttributesError
#------------------------------------------------------------------------------------------------------------------------
#run.tests ()
#------------------------------------------------------------------------------------------------------------------------
#RCytoscape:::makeRandomGraph ()
#------------------------------------------------------------------------------------------------------------------------
# this tests the otherwise invisible method in RCytocape.R, called to compensate for the extra edges and edge attributes
# packed into an undirected graph
test.remove.redundancies.in.undirected.graph = function ()
{

  print ('test.remove.redundancies.in.undirected.graph')

     # create a small random graph,
  set.seed (333)
  V = letters [1:4]
  gu = randomEGraph (V, 0.7)

    # add 2 node attributes
  nodeDataDefaults (gu, 'count') = 0
  nodeDataDefaults (gu, 'char') = 'X'


  counts = sample (1:100, length (nodes (gu)))
  chars = sample (letters, length (nodes (gu)))

  for (i in 1: length (nodes (gu))) {
    nodeData (gu, nodes (gu)[i], 'count') = counts [i]
    nodeData (gu, nodes (gu)[i], 'char')  = chars [i]
    } # for i

    # now add an edge attribute, in addition to the 'weight' attribute which randomEGraph supplies
  edgeDataDefaults (gu, 'pmid') = '9999999'
  edge.node.pairs = strsplit (edgeNames (gu), '\\~')
  for (node.pair in edge.node.pairs) {
    source.node = node.pair [1]
    target.node = node.pair [2]
    pmid.fake = 87654321 + sample (1:1000, 1)
    edgeData (gu, source.node, target.node, 'pmid') = as.character (pmid.fake)
    } # for node.pair
    
  gu.fixed = RCytoscape:::remove.redundancies.in.undirected.graph (gu)
  gu.copy = gu

    # do some basic checks:  nodes?  edges?  count of node & edge attributes?  edge attribute names?  node attribute names? 
  checkEquals (sort (nodes (gu)), sort (nodes (gu.fixed)))
  checkEquals (sort (edgeNames (gu)), sort (edgeNames (gu.fixed)))
  checkEquals (length (edgeDataDefaults (gu)), length (edgeDataDefaults (gu.fixed)))
  checkEquals (length (nodeDataDefaults (gu)), length (nodeDataDefaults (gu.fixed)))
  checkEquals (eda.names (gu), eda.names (gu.fixed))
  checkEquals (noa.names (gu), noa.names (gu.fixed))

    # now check that the default edge attribute values are all the same
  if (length (edgeDataDefaults (gu) > 0)) 
    checkTrue (all (sapply (names (edgeDataDefaults (gu)), function (eda.name) 
                checkEquals (edgeDataDefaults (gu, eda.name), edgeDataDefaults (gu.fixed, eda.name)))))

    # having checked the default eda's above, now check the specific assigned values
  edge.node.pairs = strsplit (edgeNames (gu.fixed), '\\~')
  eda.names = eda.names (gu.fixed)

  for (node.pair in edge.node.pairs) {
    source.node = node.pair [1]
    target.node = node.pair [2]
    for (edge.attribute in eda.names) {
      checkEquals (unlist (edgeData (gu,       source.node, target.node, edge.attribute), use.names=FALSE),
                   unlist (edgeData (gu.fixed, source.node, target.node, edge.attribute), use.names=FALSE))
      } # for each edge.attribute
    } # for edge

  print (1)

     # was all the node data transferred properly?
  print (2)
  if (length (nodeDataDefaults (gu)) > 0) {
    for (node in nodes (gu)) {
      for (node.attribute in noa.names (gu.fixed)) {
        checkEquals (unlist (nodeData (gu,       node, node.attribute), use.names=FALSE),
                     unlist (nodeData (gu.fixed, node, node.attribute), use.names=FALSE))
       } # for node.attribute
     } # for node
   } # if length

  invisible (gu.fixed)
  
} # test.remove.redundancies.in.undirected.graph 
#------------------------------------------------------------------------------------------------------------------------
test.randomUndirectedGraph = function ()
{
  print ('test.randomUndirectedGraph')

  g.random = RCytoscape::makeRandomGraph ()
  edgeData (g.random, '1', '2', 'weight') = 0.55
  edgeData (g.random, '1', '2', 'pmid') = '12345678' 

  cwr = new.CytoscapeWindow ('random', g.random)
  displayGraph (cwr)
  layout (cwr, 'jgraph-spring')
  redraw (cwr)

  invisible (cwr)

} # test.randomUndirectedGraph 
#------------------------------------------------------------------------------------------------------------------------
test.simpleGraph = function ()
{
  print ('test.simpleGraph')

  title = 'test.simpleGraph'
  cy = CytoscapeConnection ()

  if (title %in% as.character (getWindowList (cy)))
     destroyWindow (cy, title)

  g.simple = RCytoscape::makeSimpleGraph ()
  cws = new.CytoscapeWindow (title, g.simple)

  displayGraph (cws)
  layout (cws, 'jgraph-spring')
  setNodeLabelRule (cws, 'label')
  node.attribute.values = c ("kinase",  "transcription factor")
  colors =                c ('#0000FF', '#FF0000')
  setDefaultNodeBorderWidth (cws, 5)
  setNodeBorderColorRule (cws, 'type', node.attribute.values, colors, mode='lookup', default.color='#88FF22')
  count.control.points = c (2, 30, 100)
  sizes                = c (20, 50, 100)
  setNodeSizeRule (cws, 'count', count.control.points, sizes)
  setNodeColorRule (cws, 'lfc', c (-3.0, 0.0, 3.0), c ('#00FF00', '#FFFFFF', '#FF0000'))

  redraw (cws)

  msg (cws, title)

  invisible (cws)

} # test.simpleGraph
#------------------------------------------------------------------------------------------------------------------------
test.setGraph = function ()
{
  print ('test.setGraph')

  cw = new.CytoscapeWindow ('test.setGraph')
  checkEquals (length (nodes (getGraph (cw))), 0)
  new.graph = RCytoscape::makeSimpleGraph ()
  cw = setGraph (cw, new.graph)
  checkEquals (length (nodes (getGraph (cw))), 3)

  msg (cw, 'test.setGraph')

  invisible (cw)

} # test.setGraph 
#------------------------------------------------------------------------------------------------------------------------
test.setPosition = function ()
{
  print ('test.setPosition')

  cwe = new.CytoscapeWindow ('test.setPosition', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)

  layout (cwe, 'jgraph-spring')   # get a reasonable starting layout, with the nodes well-separate

  center.x = 200
  center.y = 200
  radius = 200
  angles = rep (seq (0, 360, 5), 3)  # sweep through full revoltion 3 times, 5 degrees at a time
    # move just the A node, swinging it around the 'center' at 200, 200.  
    # it would be nice not know more about the coordinate system than I now do, perhaps to
    # query current position on any node
  for (angle in angles) {
    angle.in.radians = angle * pi / 180
    x = center.x + (radius * cos (angle.in.radians))
    y = center.y + (radius * sin (angle.in.radians))
    setPosition (cwe, 'A', x, y)
    }

  invisible (cwe)

} # test.setPosition
#------------------------------------------------------------------------------------------------------------------------
test.getPosition = function ()
{
  print ('test.getPosition')

  cwe = new.CytoscapeWindow ('test.getPosition', graph=RCytoscape::makeSimpleGraph ())
  displayGraph (cwe)
  layout (cwe, 'jgraph-spring')
  redraw (cwe)
  xx <<- cwe
  
  layout (cwe, 'jgraph-spring')   # get a reasonable starting layout, with the nodes well-separate

     # the scheme:  get current positions, find their mean, place all the nodes there,
     # get their new positions, check to see that they are the means just set.
  
  positions <<- getPosition (cwe, c ('A', 'B', 'C'))

     # place the nodes on top of each other, at the center of their 3-cornered original layout

  center.x = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$x)))))
  center.y = as.integer (round (mean (as.integer (sapply (positions, function (pos) pos$y)))))

  setPosition (cwe, c ('A', 'B', 'C'), rep (center.x, 3), rep (center.y, 3))
  current.x = getPosition (cwe, 'A')[[1]]$x
  current.y = getPosition (cwe, 'A')[[1]]$y
  #printf ('center:  %d  %d', center.x, center.y)
  #printf ('current: %d  %d', current.x, current.y)
  
  checkEquals (current.x, center.x)
  checkEquals (current.y, center.y)

  invisible (cwe)

} # test.setPosition
#------------------------------------------------------------------------------------------------------------------------
test.haveNodeAttribute = function ()
{
  cw3 = new.CytoscapeWindow ('test.haveNodeAttribute', graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

  cyCon = CytoscapeConnection ()

  nodes.with.attribute = RCytoscape:::haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'lfc')
  checkEquals (sort (nodes.with.attribute),  c ('A', 'B', 'C'))

  checkEquals (length (RCytoscape:::haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'type')), 3)
  checkEquals (length (RCytoscape:::haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'label')), 3)
  checkEquals (length (RCytoscape:::haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'count')), 3)

  checkEquals (length (RCytoscape:::haveNodeAttribute (cyCon, nodes (getGraph (cw3)), 'bogus')), 0)

  invisible (cw3)

} # test.haveNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.haveEdgeAttribute = function ()
{
  cw3 = new.CytoscapeWindow ('test.haveEdgeAttribute', graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

  cyCon = CytoscapeConnection ()

  cy2.edgenames = as.character (cy2.edge.names (getGraph (cw3)))
  edges.with.attribute = RCytoscape:::haveEdgeAttribute (cyCon, cy2.edgenames, 'edgeType')

  checkEquals (length (edges.with.attribute), 3)
  checkTrue ("A (phosphorylates) B" %in% edges.with.attribute)
  checkTrue ("B (synthetic lethal) C" %in% edges.with.attribute)
  checkTrue ("C (undefined) A" %in% edges.with.attribute)

  checkTrue (length (RCytoscape:::haveEdgeAttribute (cyCon, cy2.edgenames, 'score')) == 3)
  checkTrue (length (RCytoscape:::haveEdgeAttribute (cyCon, cy2.edgenames, 'misc')) == 3)
  checkTrue (length (RCytoscape:::haveEdgeAttribute (cyCon, cy2.edgenames, 'bogus')) == 0)

} # test.haveEdgeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.copyNodeAttributesFromCyGraph = function ()
{
  print ('test.copyNodeAttributesFromCyGraph')

  cyCon = CytoscapeConnection ()
  window.title = 'test.copyNodeAttributesFromCyGraph'

  cw3 = new.CytoscapeWindow (window.title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

    # we can now depend upon Cytoscape holding its own version of cw3@graph 
    # in expected use, we expect that 'getGraphFromWindow' will be called, to get the nodes, edges, and both
    # node & edge attributes
    # but here, we only want to test the reliabiilty of querying the Cytoscape version of the graph for all of its node
    # attributes.  so we build a 3-node graph, *without* attributes, and pass that to copyNodeAttributesFromCyGraph, 
    # which should copy those Cytoscape graph node attributes onto the graph we pass in.
  g = new ('graphNEL', edgemode='directed')
  g = graph::addNode (c ('A', 'B', 'C'), g)
  g2 = RCytoscape:::copyNodeAttributesFromCyGraph (cyCon, getWindowID (cyCon, window.title), g)
  checkEquals (length (intersect (noa.names (g2), c ("canonicalName", "count", "label", "lfc", "type"))), 5)
  checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='canonicalName')), c ('A', 'B', 'C'))
  checkEquals (as.integer (nodeData (g2, c ('A', 'B', 'C'), attr='count')), c (2, 30, 100))
  checkEquals (as.numeric (nodeData (g2, c ('A', 'B', 'C'), attr='lfc')), c (-3,  0,  3))
  checkEquals (as.character (nodeData (g2, c ('A', 'B', 'C'), attr='type')), c ("kinase", "transcription factor", "glycoprotein"))

  invisible (cw3)

} # test.copyNodeAttributesFromCyGraph
#------------------------------------------------------------------------------------------------------------------------
test.copyEdgeAttributesFromCyGraph = function ()
{
  print ('test.copyEdgeAttributesFromCyGraph')

  cyCon = CytoscapeConnection ()
  window.title = 'test.copyEdgeAttributesFromCyGraph'
  cw3 = new.CytoscapeWindow (window.title, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

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

  g2 = RCytoscape:::copyEdgeAttributesFromCyGraph (cyCon, cw3, g)

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

  invisible (g2)

} # test.copyEdgeAttributesFromCyGraph
#------------------------------------------------------------------------------------------------------------------------
test.getGraphFromCyWindow = function ()
{
  cyCon = CytoscapeConnection ()

  cw3 = new.CytoscapeWindow ('test.getGraphFromCyWindow', graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

  g3 = getGraphFromCyWindow (cyCon, 'test.getGraphFromCyWindow')
  checkEquals (sort (nodes (g3)), c ('A', 'B', 'C'))
  checkEquals (length (intersect (noa.names (g3), c ("canonicalName", "count", "label", "lfc", "type"))), 5)
  checkEquals (as.character (sort (noa (g3, 'canonicalName'))), c ('A', 'B', 'C'))
  checkEquals (as.integer   (sort (noa (g3, 'count'))),         c (2, 30, 100))
  checkEquals (as.character (sort (noa (g3, 'label'))),         c ('Gene A', 'Gene B', 'Gene C'))
  checkEquals (as.numeric (sort (noa (g3, 'lfc'))),             c (-3,  0,  3))
  checkEquals (as.character (sort (noa (g3, 'type'))),          c ("glycoprotein", "kinase", "transcription factor"))

  checkEquals (length (intersect (eda.names (g3), c ("canonicalName", "edgeType", "interaction", "misc", "score"))), 5)

  checkEquals (sort (names (cy2.edge.names (g3))),        c ('A~B',                   'B~C',                    'C~A'))
  checkEquals (sort (as.character (cy2.edge.names (g3))), c ("A (phosphorylates) B",  "B (synthetic lethal) C", "C (undefined) A"))

  checkEquals (as.character (sort (eda (g3, 'edgeType'))), c ("phosphorylates", "synthetic lethal", "undefined"))
  checkEquals (as.character (sort (eda (g3, 'canonicalName'))), c ("A (phosphorylates) B", "B (synthetic lethal) C", "C (undefined) A"))
  checkEquals (as.character (sort (eda (g3, 'interaction'))), c ("phosphorylates", "synthetic lethal", "undefined"))
  checkEquals (as.character (sort (eda (g3, 'misc'))), c ("default misc", "default misc", "default misc"))
  checkEquals (as.numeric (sort (eda (g3, 'score'))), c (-12,  0,  35))

  invisible (g3)

} # test.getGraphFromCyWindow
#------------------------------------------------------------------------------------------------------------------------
# try graphs with no edges, then one with neither nodes nor edges
test.sendDegenerateGraphs = function ()
{
  print ('test.sendDegenerateGraphs')
  g.no.edges <<- new ('graphNEL')
  g.no.edges <<- addNode (c ('A', 'B'), g.no.edges)
  cw.degen <<- new.CytoscapeWindow ('test.sendDegenerateGraphs', g.no.edges)
  displayGraph (cw.degen)
  redraw (cw.degen)
  layout (cw.degen, 'grid')

  g.empty <<- new ('graphNEL')
  cw.empty <<- new.CytoscapeWindow ('test.sendEmptyGraph', g.empty)
  displayGraph (cw.empty)
  redraw (cw.empty)
  layout (cw.empty, 'grid')

  invisible (cw.empty)

} # test.sendDegenerateGraphs
#------------------------------------------------------------------------------------------------------------------------
test.sendBigGraph = function ()
{
  print ('test.sendBigGraph')
  probability.of.edge.being.selected = 0.0035
  node.names = as.character (1:30)
  g.big <<- randomEGraph (node.names, probability.of.edge.being.selected)
  g.big <<- initEdgeAttribute (g.big, 'weight', 'numeric', 0.0)
  write (sprintf ('graph has %d nodes and %d edges', length (nodes (g.big)), length (edgeNames (g.big))), stderr ())
  cbig <<- new.CytoscapeWindow ('test.sendBigGraph', g.big)
  stopifnot (class (cbig) == "CytoscapeWindowClass")
  displayGraph (cbig)
  redraw (cbig)
  layout (cbig, 'grid')

  invisible (cbig)

} # test.sendBigGraph
#------------------------------------------------------------------------------------------------------------------------
test.addGraphToGraph = function ()
{
  cyCon = CytoscapeConnection ()
  window.name = 'test.addGraphToGraph'
  if (window.name %in% as.character (getWindowList (cyCon)))
     destroyWindow (cyCon, window.name)

  cw3 <<- new.CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)

  g2 <<- new("graphNEL", edgemode = "directed")
  g2 <<- graph::addNode ('A', g2)
  g2 <<- graph::addNode ('B', g2)
  g2 <<- graph::addNode ('D', g2)
  g2 <<- graph::addNode ('E', g2)

  g2 <<- initNodeAttribute (g2, "label", "char", "default node label")
  g2 <<- initNodeAttribute (g2, "type", "char", "unspecified type")

  g2 <<- initNodeAttribute (g2, "SCORE", "numeric", 0.0)

  g2 <<- initEdgeAttribute (g2, "edgeType", "char", "unspecified")
  g2 <<- initEdgeAttribute (g2, "probability", "numeric", 0.0)

  nodeData (g2, 'D', 'label') <<- 'Gene D'
  nodeData (g2, 'E', 'label') <<- 'Gene E'
  nodeData (g2, 'D', 'type') <<- 'new and novel'
  nodeData (g2, 'E', 'type') <<- 'new and credible'

  nodeData (g2, 'D', 'SCORE') <<- 1001.01
  nodeData (g2, 'E', 'SCORE') <<- 99.09

  g2 <<- graph::addEdge ('D', 'E', g2)
  g2 <<- graph::addEdge ('A', 'E', g2)
  #g2 <<- graph::addEdge ('A', 'B', g2)

  edgeData (g2, 'D', 'E', 'probability') <<- 0.95
  edgeData (g2, 'D', 'E', 'edgeType') <<- 'literature'
  edgeData (g2, 'A', 'E', 'edgeType') <<- 'inferred'

  addGraphToGraph (cw3, g2)
  redraw (cw3)
  layout (cw3)

    # now copy the combined graph back to R, check it for consistency
  cw.copy <<- existing.CytoscapeWindow ('test.addGraphToGraph', copy=T)

    # first, simple node and edge names
  checkEquals (sort (nodes (cw.copy@graph)), c ('A', 'B', 'C', 'D', 'E'))
  checkEquals (sort (edgeNames (cw.copy@graph)), c ("A~B", "A~E", "B~C", "C~A", "D~E"))

    # are all the expected node and edge attributes present?
  checkEquals (length (intersect (noa.names (cw.copy@graph), c ("canonicalName", "count", "label", "lfc", "SCORE", "type"))), 6)

    # edge attributes
  checkEquals (length (intersect (eda.names (cw.copy@graph), c ("canonicalName", "edgeType", "interaction", "misc", "probability", "score"))), 6)

    # check the node label attributes
  checkEquals (nodeData (cw.copy@graph, attr='label')$A, 'Gene A')
  checkEquals (nodeData (cw.copy@graph, attr='label')$B, 'Gene B')
  checkEquals (nodeData (cw.copy@graph, attr='label')$C, 'Gene C')
  checkEquals (nodeData (cw.copy@graph, attr='label')$D, 'Gene D')
  checkEquals (nodeData (cw.copy@graph, attr='label')$E, 'Gene E')
  
    # check the edgeType attributes
  checkEquals (edgeData (cw.copy@graph, 'A', 'B', attr='edgeType')[[1]], 'phosphorylates')
  checkEquals (edgeData (cw.copy@graph, 'A', 'E', attr='edgeType')[[1]], 'inferred')
  checkEquals (edgeData (cw.copy@graph, 'B', 'C', attr='edgeType')[[1]], 'synthetic lethal')
  checkEquals (edgeData (cw.copy@graph, 'C', 'A', attr='edgeType')[[1]], 'undefined')
  checkEquals (edgeData (cw.copy@graph, 'D', 'E', attr='edgeType')[[1]], 'literature')

    # check the edge probability attributes
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'A', 'B', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'A', 'E', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'B', 'C', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'C', 'A', attr='probability')[[1]]), 0.0)
  checkEquals (as.numeric (edgeData (cw.copy@graph, 'D', 'E', attr='probability')[[1]]), 0.95)

  checkEquals (as.integer (edgeData (cw.copy@graph, 'A', 'B', attr='score')[[1]]), 35)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'A', 'E', attr='score')[[1]]), 0)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'B', 'C', attr='score')[[1]]), -12)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'C', 'A', attr='score')[[1]]), 0)
  checkEquals (as.integer (edgeData (cw.copy@graph, 'D', 'E', attr='score')[[1]]), 0)

  invisible (cw.copy)

} # test.addGraphToGraph
#------------------------------------------------------------------------------------------------------------------------
# can we create an edge attribute de novo?
# can we set its value?  retrieve its value?
test.getAttributeNames = function ()
{
  
} # test.addEdgeAttribute
#------------------------------------------------------------------------------------------------------------------------
test.addGetAndDeleteEdgeAttributes = function ()
{
  print ('test.addGetAndDeleteEdgeAttributes')

  g  = makeSimpleGraph ()
  cw = CytoscapeWindow ('test.addGetAndDeleteEdgeAttributes', graph=g)
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  redraw (cw)

  cy = CytoscapeConnection ()

     # in this test we add two new edge attributes, 'species' and 'ageInYears'
     # if they are already defined, from a previous run of this test, start by deleting them.

  novel.eda.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getEdgeAttributeNames(cy))
  for (eda.name in novel.eda.to.delete)
    deleteEdgeAttribute (cy,eda.name)
    
     # canonicalName and interaction are added by Cytoscape
  checkEquals (length (intersect (getEdgeAttributeNames (cy), c ("canonicalName", "edgeType", "interaction", "misc", "score"))), 5)

     # now add an attribute to two of the edges 
  first.two.edges = as.character (cy2.edge.names (g)[1:2])
  values = c ('hemlock', 'yew')
  sendEdgeAttributesDirect (cw, 'treeSpecies', 'char', first.two.edges, values)

    # now add an attribute to a single edge.  this exercises a different branch in RCytoscape:sendEdgeAttributesDirect
  first.edge = as.character (cy2.edge.names (g)[1])
  value = 'one century'
  sendEdgeAttributesDirect (cw, 'ageInYears', 'char', first.edge, value)
  checkTrue ('ageInYears' %in% getEdgeAttributeNames (cw))

     # get names from cy2.edge.names (cw@graph)
  checkEquals (getEdgeAttribute (cw, "B (synthetic lethal) C", 'treeSpecies'), "yew")
  checkEquals (getEdgeAttribute (cw, "B (synthetic lethal) C", 'score'), -12)

  deleteEdgeAttribute (cy, 'species')
  deleteEdgeAttribute (cy, 'ageInYears')

  invisible (cw)

} #  test.addGetAndDeleteEdgeAttributes 
#------------------------------------------------------------------------------------------------------------------------
test.addGetAndDeleteNodeAttributes = function ()
{
  print ('test.addGetAndDeleteNodeAttributes')

  cy = CytoscapeConnection ()

     # in this test we add two new node attributes, 'species' and 'ageInYears'
     # if they are already defined, from a previous run of this test, start by deleting them.

  novel.noa.to.delete = intersect (c ('ageInYears', 'treeSpecies'), getNodeAttributeNames(cy))
  for (noa.name in novel.noa.to.delete)
    deleteNodeAttribute (cy, noa.name)
    
  g  = makeSimpleGraph ()
  cw = CytoscapeWindow ('noa test', graph=g)
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  redraw (cw)

     # canonicalName is added by Cytoscape
  checkEquals (length (intersect (getNodeAttributeNames (cy), c ("canonicalName", "count", "hiddenLabel", "label", "lfc", "type"))), 6)

     # now add an attribute to two of the nodes 
  first.two.nodes = nodes (g) [1:2]
  values = c ('cedar', 'ash')
  sendNodeAttributesDirect (cw, 'treeSpecies', 'char', first.two.nodes, values)

    # now add an attribute to a single node.  this exercises a different branch in RCytoscape:sendNodeAttributesDirect
  first.node = nodes (g) [1]
  value = 'one millenium'
  sendNodeAttributesDirect (cw, 'ageInYears', 'char', first.node, value)
  checkTrue ('ageInYears' %in% getNodeAttributeNames (cw))
  checkEquals (getNodeAttribute (cw, 'B', 'type'), 'transcription factor')
  checkEquals (getNodeAttribute (cw, 'A', 'ageInYears'), 'one millenium')
  checkEquals (getNodeAttribute (cw, 'B', 'ageInYears'), '')

  deleteNodeAttribute (cy, 'species')
  deleteNodeAttribute (cy, 'ageInYears')

  invisible (cw)

} #  test.addGetAndDeleteNodeAttributes 
#------------------------------------------------------------------------------------------------------------------------
test.getVisualStyleNames = function ()
{
  print ('test.getVisualStyleNames')
  window.name = 'getVisualStyleNames'
  cw3 =  new.CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw3)
  redraw (cw3)
  layout (cw3)
  current.names = getVisualStyleNames (cw3)
  checkTrue (length (intersect (current.names, c ('Solid', 'default', 'Nested Network Style', 'Minimal', 'Sample1', 'Universe'))) >= 3)

  invisible (cw3)

} # test.getVisualStyleNames 
#------------------------------------------------------------------------------------------------------------------------
test.copyVisualStyle = function ()
{
  print ('test.copyVisualStyle, special debugging version for Mark Grimes')
  write (sprintf ('does the variable "cy" exist upon entry?  %s', exists ('cy')), stderr ())

  cy = CytoscapeConnection ()
  write (sprintf ('does the variable "cy" exist after explicit construction?  %s', exists ('cy')), stderr ())
  
  window.name = 'copyVisualStyle'
  
  write (sprintf ('does the variable "cy" exist before needed as argument to getWindowList?  %s', exists ('cy')), stderr ())
  if (window.name %in% as.character (getWindowList (cy))) {
    write (sprintf ('does the variable "cy" exist before needed as argument to destroyWindow?  %s', exists ('cy')), stderr ())
    destroyWindow (cy, window.name)
    }

  cw4 = new.CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw4)
  redraw (cw4)
  layout (cw4)

  current.names = getVisualStyleNames (cw4)

    # code around a very weird bug, which I do not understand at all (pshannon, 26 dec 2010)
  unique.name = FALSE;
  new.style.name <<-  sprintf ("tmp.%s", runif (1, 1, 1000))
  copyVisualStyle (cw4, 'default', new.style.name)
  new.names = getVisualStyleNames (cw4)
  checkEquals (setdiff (new.names, current.names), new.style.name)

  invisible (cw4)

} # test.copyVisualStyle
#------------------------------------------------------------------------------------------------------------------------
test.setVisualStyle = function ()
{
  print ('test.setVisualStyle')
  window.name = 'setVisualStyle'
  cy = CytoscapeConnection ()
  
  if (window.name %in% as.character (getWindowList (cy)))
    destroyWindow (cy, window.name)

  cw5 = new.CytoscapeWindow (window.name, graph=makeSimpleGraph ())
  displayGraph (cw5)
  redraw (cw5)
  layout (cw5)
  
  current.names = getVisualStyleNames (cw5)
  for (style.name in current.names) {
    setVisualStyle (cy, style.name)
    system ('sleep 1')
    } # for style.name

  invisible (cw5)

} # test.setVisualStyle
#------------------------------------------------------------------------------------------------------------------------
# meager test only:  make sure all of these methods can be called
# todo:  call set, call get, check for color match
test.defaultColors = function ()
{
  print ('test.defaultColors')
  cy = CytoscapeConnection ()
  getDefaultBackgroundColor (cy)
  getDefaultEdgeReverseSelectionColor (cy)
  getDefaultEdgeSelectionColor (cy)
  getDefaultNodeReverseSelectionColor (cy)
  getDefaultNodeSelectionColor (cy)

  black = '#000000'
  red = '#FF0000'
  white = '#FFFFFF'
  green = '#00FF00'
  gray = '#888888'

  setDefaultBackgroundColor (cy, white)
  setDefaultEdgeReverseSelectionColor (cy, red)
  setDefaultEdgeSelectionColor (cy, green)
  setDefaultNodeReverseSelectionColor (cy, red)
  setDefaultNodeSelectionColor (cy, green)

} # test.defaultColors
#------------------------------------------------------------------------------------------------------------------------
