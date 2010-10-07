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

  cw = CytoscapeWindow (create.window=FALSE)
  destroyAllWindows (cw)

   test.version ()
   test.create.class ()
   test.destroyWindow ()
   test.destroyAllWindows ()
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
   test.setNodeLabelRule ()
   test.setNodeTooltipRule ()
   test.setEdgeTooltipRule ()
   test.setNodeColorRule ()
   test.setNodeBorderColorRule ()
   test.setNodeSizeRule ()
   test.setNodeShapeRule ()
   test.getAllNodes ()
   test.getAllEdges ()
   test.selectNodes ()
   test.setEdgeLineStyleRule ()
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


#  test.remove.redundancies.in.undirected.graph ()
#
#  test.create.class ()
#
#  test.getWindowList ()
#  test.destroyWindow ()
#  test.destroyAllWindows ()
#  
#  test.getNodeShapes ()
#  test.getArrowShapes ()
#  test.getLineStyles ()
#  test.getLayoutNames ()
#  test.getAttributeClassNames ()
#
#  test.sendNodes ()
#  test.sendNodeAttributes ()
#
#  test.sendEdges ()
#  test.cy2.edge.names ()
#  test.sendEdgeAttributes ()
#
#  test.panelOperations ()
#
#  test.setNodeLabelRule ()
#  test.setNodeTooltipRule ()
#  test.setEdgeTooltipRule ()
#  test.setNodeTooltipRule ()
#  test.setNodeColorRule ()
#  test.setNodeBorderColorRule ()
#  test.setNodeShapeRule ()
#  test.setNodeSizeRule ()
#
#  test.getAllNodes ()
#  test.getAllEdges ()
#
#  test.selectNodes ()
#  # test.hideSelectedNodes ()   # waiting on a cy 2.8 fix
#
#  test.setEdgeLineStyleRule ()
#  test.setEdgeColorRule ()
#  test.setEdgeTargetArrowRule ()
#  test.setEdgeSourceArrowRule ()
#
#  test.movie ()
#  test.randomUndirectedGraph ()

  options ('warn'=0)

} # run.tests
#------------------------------------------------------------------------------------------------------------------------
test.version = function ()
{
  write ('test.version', stderr ())
  cw = CytoscapeWindow (create.window=FALSE)
  version.string = version (cw)
  tokens = strsplit (version.string, ' ')[[1]][1]
  version.numbers = as.integer (strsplit (tokens, '\\.')[[1]])
  major.minor.version = version.numbers [1] + (version.numbers [2]/10.0)
  msg (cw, paste ('CytoscapeRPC version', major.minor.version))
  checkTrue (major.minor.version >= 1.1)

} # test.version
#------------------------------------------------------------------------------------------------------------------------
test.create.class = function ()
{
  write ('test.create.class', stderr ())
  g = new ('graphNEL')
  cw <<- CytoscapeWindow ('unitTest', g)
  checkTrue (validObject (cw))

} # test.create.class
#------------------------------------------------------------------------------------------------------------------------
test.destroyWindow = function ()
{
  write ('test.destroyWindow', stderr ())
  cw = CytoscapeWindow ('unitTest', new ('graphNEL'))
  original.window.count = getWindowCount (cw)
  destroyWindow (cw)
  msg (cw, 'destroyed one window')
  new.window.count = getWindowCount (cw)
  checkTrue (new.window.count == original.window.count - 1)

} # test.destroyWindow
#------------------------------------------------------------------------------------------------------------------------
test.destroyAllWindows = function ()
{
  write ('test.destroyAllWindows', stderr ())
  cw = CytoscapeWindow (create.window=FALSE)
  destroyAllWindows (cw)
  new.window.count = getWindowCount (cw)
  checkEquals (new.window.count, 0)
  msg (cw, 'destroyed all windows')

} # test.destroyAllWindows
#------------------------------------------------------------------------------------------------------------------------
test.getWindowList = function ()
{
  write ('test.getWindowList', stderr ())
  test.window.name = 'windowListTest'
  cw2 <<- CytoscapeWindow (test.window.name, new ('graphNEL'))
  window.list <<- getWindowList (cw2)
  checkTrue (test.window.name %in% as.character (getWindowList (cw2)))

} # test.getWindowList
#------------------------------------------------------------------------------------------------------------------------
test.getNodeShapes = function ()
{
  write ('test.getNodeShapes', stderr ())

  cw = CytoscapeWindow (create.window=F)
  shapes = getNodeShapes (cw)
  checkTrue (length (shapes) > 10)
  msg (cw, 'getNodeShapes')

   # pick a few specific shapes to test
 checkTrue (all (sapply (c ('trapezoid', 'ellipse', 'triangle'), function (s) s %in% shapes)))

} # test.getNodeShapes
#------------------------------------------------------------------------------------------------------------------------
test.getAttributeClassNames = function ()
{
  write ('test.getAttributeClassNames', stderr ())

  x = CytoscapeWindow (create.window=F)
  possible.values = getAttributeClassNames (x)
  checkTrue (grep ('numeric', possible.values) > 0)
  checkTrue (grep ('integer', possible.values) > 0)
  checkTrue (grep ('character', possible.values) > 0)


} # test.getNodeShapes
#------------------------------------------------------------------------------------------------------------------------
test.getArrowShapes = function ()
{
  write ('test.getArrowShapes', stderr ())

  x = CytoscapeWindow (create.window=F)
  shapes = getArrowShapes (x)
  checkTrue (length (shapes) >= 8)

   # pick a few specific shapes to test
 msg (x, 'getArrowShapes')
 checkTrue (all (sapply (c ('Diamond', 'T', 'Circle'), function (s) s %in% shapes)))

} # test.getArrowShapes
#------------------------------------------------------------------------------------------------------------------------
test.getLineStyles = function ()
{
  write ('test.getLineStyles', stderr ())

  x = CytoscapeWindow (create.window=F)
  styles = getLineStyles (x)
  checkTrue (length (styles) > 10)

   # pick a few specific styles to test
  msg (x, 'getLineStyles')
  checkTrue (all (sapply (c ('SOLID', 'DOT', 'EQUAL_DASH'), function (s) s %in% styles)))

} # test.getLineStyles
#------------------------------------------------------------------------------------------------------------------------
test.getLayoutNames = function ()
{
  write ('test.getLayoutNames', stderr ())

  x = CytoscapeWindow (create.window=F)
  names = getLayoutNames (x)
  checkTrue (length (names) > 15)

   # pick a few specific styles to test
  msg (x, 'getLayoutNames')
  checkTrue (all (sapply (c ('grid', 'jgraph-spring', 'circular'), function (s) s %in% names)))

} # test.getLayoutNames
#------------------------------------------------------------------------------------------------------------------------
test.sendNodes = function ()
{
  write ('test.sendNodes', stderr ())
  g <<- RCytoscape::makeSimpleGraph ()
  cwa <<- CytoscapeWindow ('test.sendNodes', graph=g)
  sendNodes (cwa)
  layout (cwa, "grid")   # no edges, so other layouts will simply superimpose the nodes
  redraw (cwa)
  msg (cwa, 'sendNodes')

} # test.sendNodes
#------------------------------------------------------------------------------------------------------------------------
test.sendEdges = function ()
{
  write ('test.sendEdges', stderr ())
  g <<- RCytoscape::makeSimpleGraph ()
  cwe <<- CytoscapeWindow ('test.sendEdges', graph=g)
  sendNodes (cwe)
  sendEdges (cwe)
  layout (cwe, 'jgraph-circle')
  redraw (cwe)
  msg (cwe, 'sendEdges')

} # test.sendEdges
#------------------------------------------------------------------------------------------------------------------------
test.sendNodeAttributes = function ()
{
  write ('test.sendNodeAttributes', stderr ())
  g <<- RCytoscape::makeSimpleGraph ()
  cwb <<- CytoscapeWindow ('test.sendNodeAttributes', graph=g)
  sendNodes (cwb)
  attribute.names = noa.names (g)

  for (attribute.name in attribute.names) {
    result = sendNodeAttributes (cwb, attribute.name)
    }

  layout (cwb, 'grid')
  redraw (cwb)
  msg (cwb, 'sendNodeAttributes')

} # test.sendNodeAttributes
#------------------------------------------------------------------------------------------------------------------------
# depends on prior creation of cwe by test.sendEdges
test.sendEdgeAttributes = function ()
{
  write ('test.sendEdgeAttributes', stderr ())
  attribute.names = eda.names (cwe@graph)

  for (attribute.name in attribute.names) {
    result = sendEdgeAttributes (cwe, attribute.name)
    } 

  edge.names = as.character (cy2.edge.names (cwe@graph))
  checkEquals (length (edge.names), 3)
  edge.values = c ('alligator', 'hedgehog', 'anteater')
  result = sendEdgeAttributesDirect (cwe, 'misc', 'string', edge.names, edge.values)
  msg (cwe, 'sendEdgeAttributes')

} # test.sendEdgeAttributes
#------------------------------------------------------------------------------------------------------------------------
test.cy2.edge.names = function ()
{
  write ('test.cy2.edge.names', stderr ())
  g <<- RCytoscape::makeSimpleGraph ()

    # this graph has the expected 'edgeType' edge attribute, used to make a standard cytoscape edge name
  edge.names <<- cy2.edge.names (g)
  checkEquals (edge.names [['A~B']], "A (phosphorylates) B")
  checkEquals (edge.names [['B~C']], "B (synthetic lethal) C")
  checkEquals (edge.names [['C~A']], "C (undefined) A")

    # now create a tiny graph, two nodes, one edge, with NO edgeType attribute.  make sure it is converted properly
  g2 =  new ('graphNEL', edgemode='directed')

  g2 = graph::addNode ('A', g2)
  g2 = graph::addNode ('B', g2)
  g2 = graph::addEdge ('A', 'B', g2)

  edge.names.2 <<- cy2.edge.names (g2)  
  checkEquals (edge.names.2 [['A~B']], "A (unknown) B")

} # test.cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
# depends on prior creation of cw by test.createClass, providing a CytoscapeWindow object, with a 'uri' slot
test.panelOperations = function ()
{
  if (!exists ('cw')) {
    cw <<- CytoscapeWindow ('test.panelOperations')
    }

  hidePanel (cw, 'Control Panel')
  hidePanel (cw, 'd')

  floatPanel (cw, 'Control Pa')
  floatPanel (cw, 'DATA')

  dockPanel (cw, 'control ')
  dockPanel (cw, 'data panel')
  msg (cw, 'test.panelOperations')

} # test.panelOperations
#------------------------------------------------------------------------------------------------------------------------
test.setNodeLabelRule = function ()
{
  write ('test.setNodeLabelRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeLabelRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

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

}  # test.setNodeLabelRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeTooltipRule = function ()
{
  write ('test.setNodeTooltipRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeTooltiprRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  #setNodeLabelRule (cwe, 'label')
  setNodeTooltipRule (cwe, 'type')
  #setNodeLabelRule (cwe, 'lfc')
  #setNodeLabelRule (cwe, 'count')
  #setNodeLabelRule (cwe, 'label')
  msg (cwe, 'test.setNodeTooltipRule')

}  # test.setNodeTooltipRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTooltipRule = function ()
{
  write ('test.setEdgeTooltipRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setEdgeTooltiprRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  hidePanel (cwe, 'c');  hidePanel (cwe, 'd');
  setEdgeTooltipRule (cwe, 'edgeType')
  msg (cwe, 'test.setEdgeTooltipRule')

}  # test.setEdgeTooltipRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeColorRule = function ()
{
  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeColorRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

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

} # test.setNodeColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeBorderColorRule = function ()
{
  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeBorderColorRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

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

  msg (cwe, 'test.setNodeBorderColorRule')

} # test.setNodeBorderColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeSizeRule = function ()
{
  write ('test.setNodeSizeRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeSizeRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

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

} # test.setNodeSizeRule
#------------------------------------------------------------------------------------------------------------------------
test.setNodeShapeRule = function ()
{
  write ('test.setNodeShapeRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeShapeRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  node.attribute.name = 'type'
  attribute.values = as.character (noa (getGraph (cwe), 'type'))
  checkEquals (length (attribute.values), 3)
  for (i in 1:2) {
    node.shapes = getNodeShapes (cwe) [sample (length (getNodeShapes (cwe)), 3)]
    setNodeShapeRule (cwe, node.attribute.name='type', attribute.values, node.shapes)
    } # for i

  msg (cwe, 'test.setNodeShapeRule')

} # test.setNodeShapeRule
#------------------------------------------------------------------------------------------------------------------------
test.getAllNodes = function ()
{
  write ('test.getAllNodes', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.getAllNodes', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }


  cwe.nodes <<- getAllNodes (cwe)
  checkEquals (length (intersect (cwe.nodes, nodes (cwe@graph))), 3)

  msg (cwe, 'test.getAllNodes')

} # test.getAllNodes
#------------------------------------------------------------------------------------------------------------------------
test.getAllEdges = function ()
{
  write ('test.getAllEdges', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.getAllEdges', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  cwe.edges <<- getAllEdges(cwe)
  checkTrue ("C (undefined) A" %in% cwe.edges)
  checkTrue ("B (synthetic lethal) C" %in% cwe.edges)
  checkTrue ("A (phosphorylates) B" %in% cwe.edges)

  msg (cwe, 'test.getAllEdges')

} # test.getAllEdges
#------------------------------------------------------------------------------------------------------------------------
test.selectNodes = function ()
{
  write ('test.selectNodes', stderr ())
  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.selectNodes', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  clearSelection (cwe)
  checkEquals (getSelectedNodeCount (cwe), 0)
  cwe.nodes = selectNodes (cwe, c ('A', 'B'))
  checkEquals (getSelectedNodeCount (cwe), 2)
  clearSelection (cwe)
  msg (cwe, 'test.selectNodes')

} # test.selectNodes
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeLineStyleRule = function ()
{
  write ('test.setEdgeLineStyleRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setEdgeLineStyleRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }


  line.styles = c ('SINEWAVE', 'DOT', 'PARALLEL_LINES')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (line.styles, getLineStyles (cwe))), 3)

  setEdgeLineStyleRule (cwe, 'edgeType', edgeType.values, line.styles)
  msg (cwe, 'test.setEdgeLineStyleRule')

} # test.setEdgeLineStyleRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeColorRule = function ()
{
  write ('test.setEdgeColorRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setEdgeColorRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  colors = c ('#FF0000', '#FFFF00', '#00FF00')
  setEdgeColorRule (cwe, 'edgeType',  edgeType.values, colors)
  system ('sleep 1')
  all.white  = c ('#FFFFFF', '#FFFFFF', '#FFFFFF')
  setEdgeColorRule (cwe, 'edgeType',  edgeType.values, all.white)

  msg (cwe, 'test.setEdgeColorRule')

} # test.setEdgeColorRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeTargetArrowRule = function ()
{
  write ('test.setEdgeTargetArrowRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setEdgeTargetArrowRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }


  arrows = c ('Delta', 'T', 'Diamond')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 3)

  setEdgeTargetArrowRule (cwe, 'edgeType', edgeType.values, arrows)

  msg (cwe, 'test.setEdgeTargetArrowRule')

} # test.setEdgeTargetArrowRule
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeArrowColorRules = function ()
{
  write ('test.setEdgeArrowColorRules', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setEdgeArrowColorRules', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

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

  msg (cwe, 'test.setEdgeArrowColorRules')

} # test.setEdgetArrowColorRules
#------------------------------------------------------------------------------------------------------------------------
test.setEdgeSourceArrowRule = function ()
{
  write ('test.setEdgeSourceArrowRule', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setSourceArrowRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }


  arrows = c ('Arrow', 'Diamond', 'Circle')
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  checkEquals (length (intersect (arrows, getArrowShapes (cwe))), 3)

  setEdgeSourceArrowRule (cwe, 'edgeType', edgeType.values, arrows)

  msg (cwe, 'test.setEdgeSourceArrowRule')

} # test.setEdgeSourceArrowRule
#------------------------------------------------------------------------------------------------------------------------
test.movie = function ()
{
  write ('test.movie', stderr ())

  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.movie', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }



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

} # test.movie
#------------------------------------------------------------------------------------------------------------------------
test.unmatchedAttributesError = function ()
{
  print ('test.unmatchedAttributesError')
  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('unmatched attributes error', RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

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
    
  gu.fixed <<- RCytoscape:::remove.redundancies.in.undirected.graph (gu)
  gu.copy <<- gu

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

   return (TRUE)
  
} # test.remove.redundancies.in.undirected.graph 
#------------------------------------------------------------------------------------------------------------------------
test.randomUndirectedGraph = function ()
{
  print ('test.randomUndirectedGraph')

  g.random = RCytoscape::makeRandomGraph ()
  edgeData (g.random, '1', '2', 'weight') = 0.55
  edgeData (g.random, '1', '2', 'pmid') = '12345678' 

  cwr <<- CytoscapeWindow ('random', g.random)
  displayGraph (cwr)
  layout (cwr, 'jgraph-spring')
  redraw (cwr)

} # test.randomUndirectedGraph 
#------------------------------------------------------------------------------------------------------------------------
test.simpleGraph = function ()
{
  print ('test.simpleGraph')

  g.simple <<- RCytoscape::makeSimpleGraph ()
  #attr (edgeDataDefaults (g.random, attr="weight"), "class") <<- "FLOAT"
  cws <<- CytoscapeWindow ('simple', g.simple)
  displayGraph (cws)
  layout (cws, 'jgraph-spring')
  redraw (cws)

  msg (cwe, 'test.simpleGraph')


} # test.simpleGraph
#------------------------------------------------------------------------------------------------------------------------
test.setGraph = function ()
{
  print ('test.simpleGraph')

  cw <<- CytoscapeWindow ('initially empty')
  checkEquals (length (nodes (getGraph (cw))), 0)
  new.graph = RCytoscape::makeSimpleGraph ()
  cw <<- setGraph (cw, new.graph)
  checkEquals (length (nodes (getGraph (cw))), 3)

  msg (cwe, 'test.setGraph')

} # test.setGraph 
#------------------------------------------------------------------------------------------------------------------------
test.setPosition = function ()
{
  if (!exists ('cwe')) {
    cwe <<- CytoscapeWindow ('test.setNodeLabelRule', graph=RCytoscape::makeSimpleGraph ())
    displayGraph (cwe)
    layout (cwe, 'jgraph-spring')
    redraw (cwe)
    }

  set.seed (123)
  layout (cwe, 'jgraph-spring')   # get a reasonable starting layout, with the nodes well-separate

  for (i in 1:50) {    # first do the single node case
    x = 5 * runif (1)
    y = 5 * runif (1)
    setPosition (cwe, nodes (getGraph (cwe)) [1], x, y)
    } # for i

  for (i in 1:5) {    # now do all the nodes in every call
    x = runif (3, 1, 100)
    y = runif (3, 1, 100)
    setPosition (cwe, nodes (getGraph (cwe)), x, y)
    } # for i

} # test.setPosition
#------------------------------------------------------------------------------------------------------------------------