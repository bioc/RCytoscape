library (graph)
library (XMLRPC)
library (methods)
#------------------------------------------------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
#------------------------------------------------------------------------------------------------------------------------
setClass ("CytoscapeWindowClass", 
          representation = representation (title="character",
                                           window.id='integer',
                                           graph="graph", 
                                           uri="character"),
          prototype = prototype (title="R graph", 
                                 graph=new ("graphNEL", edgemode='directed'),
                                 uri="http://localhost:9000")
          )

#------------------------------------------------------------------------------------------------------------------------
setGeneric ('ping',                     signature='obj', function (obj) standardGeneric ('ping'))
setGeneric ('createWindow',             signature='obj', function (obj) standardGeneric ('createWindow'))
setGeneric ('getWindowCount',           signature='obj', function (obj) standardGeneric ('getWindowCount'))
setGeneric ('getWindowList',            signature='obj', function (obj) standardGeneric ('getWindowList'))
setGeneric ('destroyWindow',            signature='obj', function (obj) standardGeneric ('destroyWindow'))
setGeneric ('destroyAllWindows',        signature='obj', function (obj) standardGeneric ('destroyAllWindows'))
setGeneric ('getArrowShapes',           signature='obj', function (obj) standardGeneric ('getArrowShapes'))
setGeneric ('getLayoutNames',           signature='obj', function (obj) standardGeneric ('getLayoutNames'))
setGeneric ('getLineStyles',            signature='obj', function (obj) standardGeneric ('getLineStyles'))
setGeneric ('getNodeShapes',            signature='obj', function (obj) standardGeneric ('getNodeShapes'))
setGeneric ('getAttributeClassNames',   signature='obj', function (obj) standardGeneric ('getAttributeClassNames'))
setGeneric ('sendNodes',                signature='obj', function (obj) standardGeneric ('sendNodes'))
setGeneric ('sendEdges',                signature='obj', function (obj) standardGeneric ('sendEdges'))
setGeneric ('sendNodeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('sendNodeAttributes'))
setGeneric ('sendNodeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, node.names, values) standardGeneric ('sendNodeAttributesDirect'))
setGeneric ('sendEdgeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('sendEdgeAttributes'))
setGeneric ('sendEdgeAttributesDirect', signature='obj', 
   function (obj, attribute.name, attribute.type, edge.names, values) standardGeneric ('sendEdgeAttributesDirect'))
setGeneric ('displayGraph',             signature='obj', function (obj) standardGeneric ('displayGraph'))
setGeneric ('layout',                   signature='obj', function (obj, layout.name) standardGeneric ('layout'))
setGeneric ('redraw',                   signature='obj', function (obj) standardGeneric ('redraw'))
setGeneric ('hidePanel',                signature='obj', function (obj, panelName) standardGeneric ('hidePanel'))
setGeneric ('dockPanel',                signature='obj', function (obj, panelName) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',               signature='obj', function (obj, panelName) standardGeneric ('floatPanel'))
setGeneric ('setNodeLabelRule',         signature='obj', function (obj, node.attribute.name) standardGeneric ('setNodeLabelRule'))
setGeneric ('setNodeColorRule',         signature='obj', function (obj, node.attribute.name, control.points, colors) standardGeneric ('setNodeColorRule'))
setGeneric ('setNodeShapeRule',         signature='obj', 
     function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') standardGeneric ('setNodeShapeRule'))
setGeneric ('setNodeSizeRule',          signature='obj', 
     function (obj, node.attribute.name, attribute.values, node.sizes) standardGeneric ('setNodeSizeRule'))
setGeneric ('setEdgeLineStyleRule',     signature='obj', 
     function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') standardGeneric ('setEdgeLineStyleRule'))
setGeneric ('setEdgeTargetArrowRule',   signature='obj', 
     function (obj, edge.attribute.name, attribute.values, arrows, default='BLACK_ARROW') standardGeneric ('setEdgeTargetArrowRule'))
setGeneric ('setEdgeSourceArrowRule',   signature='obj', 
     function (obj, edge.attribute.name, attribute.values, arrows, default='BLACK_ARROW') standardGeneric ('setEdgeSourceArrowRule'))
setGeneric ('setEdgeColorRule',         signature='obj', function (obj, attribute.name, attribute.values, colors) standardGeneric ('setEdgeColorRule'))
setGeneric ('getAllNodes',              signature='obj', function (obj) standardGeneric ('getAllNodes'))
setGeneric ('getAllEdges',              signature='obj', function (obj) standardGeneric ('getAllEdges'))
setGeneric ('selectNodes',              signature='obj', function (obj, node.names) standardGeneric ('selectNodes'))
setGeneric ('sfn',                      signature='obj', function (obj) standardGeneric ('sfn'))
setGeneric ('getSelectedNodes',         signature='obj', function (obj) standardGeneric ('getSelectedNodes'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedNodeCount',     signature='obj', function (obj) standardGeneric ('getSelectedNodeCount'))
setGeneric ('hideSelectedNodes',        signature='obj', function (obj) standardGeneric ('hideSelectedNodes'))
setGeneric ('unhideAll',                signature='obj', function (obj) standardGeneric ('unhideAll'))

setGeneric ('firstNeighbors',           signature='obj', function (obj, nodeName) standardGeneric ('firstNeighbors'))
setGeneric ('sfn',                      signature='obj', function (obj) standardGeneric ('sfn'))
#------------------------------------------------------------------------------------------------------------------------
setValidity ("CytoscapeWindowClass",

  function (obj) {
    if (length (obj@title) != 1) 
      "'title' is not a single string" 
    else if (!nzchar (obj@title))
      "'title' is an empty string" 
    validObject (obj@graph)
    })

#------------------------------------------------------------------------------------------------------------------------
# the class constructor, defined as a simple function, with no formal link to the class
CytoscapeWindow = function (title='default', graph=new('graphNEL', edgemode='directed'), host='localhost', rpcPort=9000, create.window=TRUE)
{
  uri = sprintf ('http://%s:%s', host, rpcPort)

    # add a label to each node if not already present.  default label is the node name, the node ID
  if (edgemode (graph) == 'undirected') 
    graph = remove.redundancies.in.undirected.graph (graph)

  if (! 'label' %in% noa.names (graph)) {
    write ('nodes have no label attribute -- adding default labels', stderr ())
    nodeDataDefaults (graph, 'label') = ''
    attr (nodeDataDefaults (graph, attr = "label"), "class") = "STRING"
    if (length (nodes (graph) > 0))
      for (node in nodes (graph))
        nodeData (graph, node, 'label') = node
    } # if no label node attribute

  cw = new ('CytoscapeWindowClass', title=title, graph=graph, uri=uri)
  if (create.window)
    cw@window.id = createWindow (cw)
  return (cw)

} # CytsoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
setMethod ('ping', 'CytoscapeWindowClass', 
  function (obj) { 
    return (xml.rpc (obj@uri, 'Cytoscape.test'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('createWindow', 'CytoscapeWindowClass',
  function (obj) {
    window.id = as.integer (xml.rpc (obj@uri, 'Cytoscape.createNetwork', obj@title, .convert=TRUE))
    #write (sprintf ('createWindow, id = %d', window.id), stderr ()) 
    return (window.id)
  })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowCount', 'CytoscapeWindowClass',
  function (obj) {
    return (as.integer (xml.rpc (obj@uri, 'Cytoscape.getNetworkCount')))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowList', 'CytoscapeWindowClass',

  function (obj) {
    if (getWindowCount (obj) == 0)
      return (c ())

  result.raw = xml.rpc (obj@uri, 'Cytoscape.getNetworkList')
  result = c ()

  for (i in 1:length (result.raw)) {
    id = result.raw [[i]]$networkID
    title = result.raw [[i]]$networktitle
    result [[id]] = title
    } # for i

  return (result)

  }) # getWindowList
#------------------------------------------------------------------------------------------------------------------------
setMethod ('destroyWindow',  'CytoscapeWindowClass',

  function (obj) {
    id = as.character (obj@window.id)
    xml.rpc (obj@uri, 'Cytoscape.destroyNetwork', id)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('destroyAllWindows',  'CytoscapeWindowClass',

  function (obj) {
    ids = names (getWindowList (obj))
    invisible (sapply (ids, function (id)  xml.rpc (obj@uri, 'Cytoscape.destroyNetwork', id)))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'CytoscapeWindowClass',

  function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getNodeShapeNames'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAttributeClassNames', 'CytoscapeWindowClass',

  function (obj) {
     return (c ('floating|numeric|double', 'integer|int', 'string|char|character'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLineStyles', 'CytoscapeWindowClass',

  function (obj) {
    return (xml.rpc (obj@uri, 'Cytoscape.getLineStyleNames'))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getArrowShapes', 'CytoscapeWindowClass',

   function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getArrowShapeNames'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutNames', 'CytoscapeWindowClass', 

   function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getLayoutNames'))
     }) # getLayoutNames

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendNodes', 'CytoscapeWindowClass',

  function (obj) {
     if (length (nodes (obj@graph)) == 0)
       write ('CytoscapeWindow.send, no nodes in graph.  returning', stderr ())
     xml.rpc (obj@uri, 'Cytoscape.createNodes', as.character (obj@window.id), nodes (obj@graph))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdges', 'CytoscapeWindowClass',

  function (obj) {
    for (source.node in names (edges (obj@graph))) {
      for (target.node in edges (obj@graph)[[source.node]]) {
        interaction = 'unknown'
        if ('edgeType' %in% names (edgeDataDefaults (obj@graph)))
          interaction = as.character (edgeData (obj@graph, source.node, target.node, 'edgeType'))
        else if ('type' %in% names (edgeDataDefaults (obj@graph)))
          interaction = as.character (edgeData (obj@graph, source.node, target.node, 'type'))
        #printf ('creating edge  %s (%s) %s', source.node, interaction, target.node)     
        xml.rpc (obj@uri, 'Cytoscape.createEdge', source.node, target.node, interaction, TRUE)
        } # for target.node
      } # for source.node
    }) # sendEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod ('layout', 'CytoscapeWindowClass',

  function (obj, layout.name='jgraph-spring') {

    if (!layout.name %in% getLayoutNames (obj)) {
      write (sprintf ("layout '%s' is not recognized; call getLayoutNames (<CytoscapeWindow>) to see those which are supported", layout.name), stderr ())
      return ()
      }

    id = as.character (obj@window.id)
    return (xml.rpc (obj@uri, 'Cytoscape.performLayout', id, layout.name))
    }) # cy.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendNodeAttributes', 'CytoscapeWindowClass',

   function (obj, attribute.name) {

     caller.specified.attribute.class = attr (nodeDataDefaults (obj@graph, attribute.name), 'class')

     if (is.null (caller.specified.attribute.class)) {
       msg1 = sprintf ('Error!  RCytoscape::sendNodeAttributes. You must specify the class of the "%s" node attribute.', attribute.name)
       msg2 = sprintf ('        example: attr (nodeDataDefaults (yourGraph, attr="%s"), "class") = "STRING|DOUBLE|INTEGER"', attribute.name)
       write (msg1, stderr ())
       write (msg2, stderr ())
       return (NA)
       }

     caller.specified.attribute.class = caller.specified.attribute.class
     node.names = nodes (obj@graph)
     values = noa (obj@graph, attribute.name)
     return (sendNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, node.names, values))
     }) # sendNodeAttributes

#------------------------------------------------------------------------------------------------------------------------
# with this version, unlike sendNodeAttributes, the attributes need not be already stored in the graph
setMethod ('sendNodeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, node.names, values) {

     if (length (node.names) != length (values)) {
       write (sprintf ('RCytoscape::sendNodeAttributesDirect ERROR.'), stderr ())
       write ('attribute name %s, edge.names %d, values %d', attribute.name, length (node.names), length (values), stderr ())
       return ();
       }


     caller.specified.attribute.class = tolower (attribute.type)
     if (is.null (caller.specified.attribute.class) || length (caller.specified.attribute.class) == 0)   # NULL, or non-null but empty
       caller.specified.attribute.class = 'string'

     result = ''

     if (caller.specified.attribute.class %in% c ('float', 'floating', 'numeric', 'double')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addDoubleNodeAttributes', attribute.name, node.names, as.numeric (values), .convert=TRUE)
       write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('integer', 'int')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addIntegerNodeAttributes', attribute.name, node.names, as.integer (values), .convert=TRUE)
       write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('string', 'char', 'character')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addStringNodeAttributes', attribute.name, node.names, as.character (values), .convert=TRUE)
       write (result, stderr ())
       }

     return (result)
     }) # sendNodeAttributesDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdgeAttributes', 'CytoscapeWindowClass',

   function (obj, attribute.name) {

     caller.specified.attribute.class = attr (edgeDataDefaults (obj@graph, attribute.name), 'class')
     
     if (is.null (caller.specified.attribute.class)) {
       msg1 = sprintf ('Error!  RCytoscape::sendEdgeAttributes. You must specify the class of the "%s" edge attribute.', attribute.name)
       msg2 = sprintf ('        example: attr (edgeDataDefaults (yourGraph, attr="%s"), "class") = "STRING|DOUBLE|INTEGER"', attribute.name)
       write (msg1, stderr ())
       write (msg2, stderr ())
       return (NA)
       }
     edge.names = as.character (cy2.edge.names (obj@graph))
     values = eda (obj@graph, attribute.name)

     #write (edge.names, stderr ())
     #write (values, stderr ())

     return (sendEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, edge.names, values))
     }) # sendEdgeAttributes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdgeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, edge.names, values) {

     if (length (values) == 2 * length (edge.names))
       values = values [1:length (edge.names)]

     caller.specified.attribute.class = tolower (attribute.type)

     if (is.null (caller.specified.attribute.class) || length (caller.specified.attribute.class) == 0)   # NULL, or non-null but empty
       caller.specified.attribute.class = 'string'

     result = ''

     if (length (edge.names) != length (values)) {
       write (sprintf ('RCytoscape::sendEdgeAttributesDirect ERROR.'), stderr ())
       write ('attribute name %s, edge.names %d, values %d', attribute.name, length (edge.names), length (values), stderr ())
       return ();
       }

     if (caller.specified.attribute.class %in% c ('floating', 'numeric', 'double')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addDoubleEdgeAttributes', attribute.name, edge.names, as.numeric (values), .convert=TRUE)
       write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('integer', 'int')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addIntegerEdgeAttributes', attribute.name, edge.names, as.integer (values), .convert=TRUE)
       write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('string', 'char', 'character')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addStringEdgeAttributes', attribute.name, edge.names, as.character (values), .convert=TRUE)
       write (result, stderr ())
       }

     return (result)
     }) # sendEdgeAttributesDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('displayGraph', 'CytoscapeWindowClass',

   function (obj) {
     write ('adding nodes...', stderr ())
     sendNodes (obj)
     write ('adding edges...', stderr ())
     sendEdges (obj)
     write ('adding node attributes...', stderr ())
     sapply (noa.names (obj@graph), function (name) {print (name); sendNodeAttributes (obj, name)})
     write ('adding edge attributes...', stderr ())
     sapply (eda.names (obj@graph), function (name) {print (name);  sendEdgeAttributes (obj, name)})
     }) # displayGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod ('redraw', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.redraw', id))
     }) # redraw

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hidePanel', 'CytoscapeWindowClass',

   function (obj, panelName) {
     xml.rpc (obj@uri, 'Cytoscape.hidePanel', panelName)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('dockPanel', 'CytoscapeWindowClass',

   function (obj, panelName) {
     xml.rpc (obj@uri, 'Cytoscape.dockPanel', panelName)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('floatPanel', 'CytoscapeWindowClass',

   function (obj, panelName) {
     xml.rpc (obj@uri, 'Cytoscape.floatPanel', panelName)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelRule', 'CytoscapeWindowClass',

  function (obj, node.attribute.name) {
    id = as.character (obj@window.id)
    xml.rpc (obj@uri, 'Cytoscape.setNodeLabel', id, node.attribute.name, 'label', 'default'); 
    redraw (obj)
    })  # setNodeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, colors) {

     if (!(length (control.points) == 3 & length (colors) == 3)) {
       write (sprintf ('cp: %d', length (control.points)), stderr ())
       write (sprintf ('co: %d', length (colors)), stderr ())
       write ("Error! RCytoscape:setNodeColorRule.  No support yet for node.color rules with anything other than 3 control points", stderr ())
       return ()
       }

     min.value = control.points [1]
     mid.value = control.points [2]
     max.value = control.points [3]

     min.color = colors [1]
     mid.color = colors [2]
     max.color = colors [3]
     
     result = xml.rpc (obj@uri, "Cytoscape.createContinuousNodeColorVisualStyle", node.attribute.name, 'Node Color', 
                       min.color, mid.color, max.color, min.value, mid.value, max.value, .convert=TRUE)
     invisible (result)
     }) # setNodeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setNodeShapeRule", id, node.attribute.name, default.shape, 
                      attribute.values, node.shapes, .convert=TRUE)
     return (result)
     }) # setNodeShapeRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeSizeRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, node.sizes) {
     id = as.character (obj@window.id)
        # take the first and last node size, prepend and append them respectively, so that there are 2 more
        # visual attribute values (in this case, node size in pixels) than there are node data attribute values
     adjusted.node.sizes = c (node.sizes [1], node.sizes, c (node.sizes [length (node.sizes)]))
     result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Size', 
                       attribute.values, adjusted.node.sizes)
     return (result)
     }) # setNodeSizeRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorRule', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.values, colors) {
     id = as.character (obj@window.id)
     default.color = '#000000'
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeColorRule", id, attribute.name, default.color, attribute.values, colors, .convert=TRUE)
     return (result)
     }) # set.edge.color.rule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeLineStyleRule", id, edge.attribute.name, default.style, 
                       attribute.values, line.styles, .convert=TRUE)
     return (result)
     }) # set.edge.line.style.rule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='BLACK_ARROW') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeTargetArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     return (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='BLACK_ARROW') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeSourceArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     return (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
       # todo:  getting all nodes should be inherently a window-specific operation
     result = xml.rpc (obj@uri, "Cytoscape.getAllNodes", .convert=TRUE)
     return (result)
     }) # getAllNodes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.getAllEdges", .convert=TRUE)
     return (result)
     }) # getAllEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod ('selectNodes', 'CytoscapeWindowClass',

   function (obj, node.names) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.selectNodes',id, node.names, .convert=TRUE)
     redraw (obj)
     return (result)
     }) # selectNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     if (getSelectedNodeCount (obj) == 0)
       return (NA)
     else {
       result = xml.rpc (obj@uri, 'Cytoscape.getSelectedNodes', id, .convert=TRUE)
       return (result)
       }
     }) # getSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('clearSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.clearSelection', id, .convert=TRUE)
     redraw (obj)
     return (result)
     }) # clearSelection
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedNodeCount', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.countSelectedNodes', id, .convert=TRUE))
     }) # countSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.hideSelectedNodes', id, .convert=TRUE))
     }) # hideSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('unhideAll', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.unhideAll', id, .convert=TRUE))
     }) # unhideAll
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('firstNeighbors', 'CytoscapeWindowClass',

   function (obj, nodeName) {
      if (! nodeName %in% nodes (obj@graph))
         return (NA)
      right.neighbors = edges (obj@graph)[[nodeName]]
      left.neighbors = inEdges (nodeName, obj@graph)[[1]]
      return (list (right=right.neighbors, left=left.neighbors))
      })  # firstNeighbors

#------------------------------------------------------------------------------------------
setMethod ('sfn', 'CytoscapeWindowClass',

   function (obj) {
     if (getSelectedNodeCount (obj) > 0) {
       currently.selected = getSelectedNodes (obj)
       neighbors = c (currently.selected)
       for (node in currently.selected) { 
         new.neighbors = firstNeighbors (obj, node)
         neighbors = c (neighbors, new.neighbors$right)
         neighbors = c (neighbors, new.neighbors$left)
         } # for node
       neighbors = unique (neighbors)
       if (length (neighbors) > 0)
         selectNodes (obj, neighbors)
       } # if any nodes are already selected
     }) # sfn

#------------------------------------------------------------------------------------------------------------------------
noa.names = function (graph)
{
  return (names (nodeDataDefaults (graph)))
}
#------------------------------------------------------------------------------------------------------------------------
eda.names = function (graph)
{
  return (names (edgeDataDefaults (graph)))
}
#------------------------------------------------------------------------------------------------------------------------
noa = function (graph, node.attribute.name)
{
  unlist (sapply (nodes (graph), function (x)  (nodeData (graph, x, node.attribute.name))))

} # eda
#------------------------------------------------------------------------------------------------------------------------
eda = function (graph, edge.attribute.name)
{
  unlist (sapply (names (edgeData (graph)), function (n) edgeData (graph)[[n]][[edge.attribute.name]]))

} # eda
#------------------------------------------------------------------------------------------------------------------------
#  use the expected 'edgeType' attribute to create cytoscape-style 'A (edgeType) B' edge names from a graphNEL
#  edgeNames (g) # "A~B" "B~C" "C~A"
#  if there is no edge attribute named 'edgeType', then create edges (uninterestingly) named 'A (edge) B'
cy2.edge.names = function (graph)
{
  edgeType.attribute.present = TRUE
  if (!'edgeType' %in% names (edgeDataDefaults (graph))) {
     edge.type = 'unknown'
     edgeType.attribute.present = FALSE
     }

  edge.names = c ()

  for (edge in edgeNames (graph)) {
    stopifnot (grep ('~', edge) == 1)
    tokens = strsplit (edge, '~') [[1]]
    source.node = tokens [1]
    target.node = tokens [2]
    if (edgeType.attribute.present)
      edge.type = edgeData (graph, source.node, target.node, 'edgeType')
    edge.name = paste (source.node, ' (', edge.type, ') ', target.node, sep='')
    edge.names = c (edge.names, edge.name)
    } # for edge

  names (edge.names) = edgeNames (graph)
  return (edge.names)

} # cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
makeSimpleGraph = function ()
{
  g = new ('graphNEL', edgemode='directed')
  nodeDataDefaults (g, attr='type') = 'undefined'
  attr (nodeDataDefaults (g, attr='type'), 'class') = 'STRING'

  nodeDataDefaults (g, attr='lfc') = 1.0
  attr (nodeDataDefaults (g, attr='lfc'), 'class') = 'DOUBLE'

  nodeDataDefaults (g, attr='label') = 'default node label'
  attr (nodeDataDefaults (g, attr='label'), 'class') = 'STRING'

  nodeDataDefaults (g, attr='count') = '0'
  attr (nodeDataDefaults (g, attr='count'), 'class') = 'INTEGER'

  edgeDataDefaults (g, attr='edgeType') = 'undefined'
  attr (edgeDataDefaults (g, attr='edgeType'), 'class') = 'STRING'

  edgeDataDefaults (g, attr='score') = 0.0
  attr (edgeDataDefaults (g, attr='score'), 'class') = 'DOUBLE'

  edgeDataDefaults (g, attr='misc') = 'default misc'
  attr (edgeDataDefaults (g, attr='misc'), 'class') = 'STRING'

    # from src/tudelft/CytoscapeRPC/CyAttributeAdderImpl
    #    STRING("STRING"),
    #    INTEGER("INTEGER"),
    #    FLOATING("FLOATING"),
    #    BOOLEAN("BOOLEAN"),
    #    LIST("SIMPLE_LIST"),
    #    MAP("SIMPLE_MAP");
  g = graph::addNode ('A', g)
  g = graph::addNode ('B', g)
  g = graph::addNode ('C', g)
  nodeData (g, 'A', 'type') = 'kinase'
  nodeData (g, 'B', 'type') = 'transcription factor'
  nodeData (g, 'C', 'type') = 'glycoprotein'

  nodeData (g, 'A', 'lfc') = '-3.0'
  nodeData (g, 'B', 'lfc') = '0.0'
  nodeData (g, 'C', 'lfc') = '3.0'

  nodeData (g, 'A', 'count') = '2'
  nodeData (g, 'B', 'count') = '30'
  nodeData (g, 'C', 'count') = '100'

  nodeData (g, 'A', 'label') = 'Gene A'
  nodeData (g, 'B', 'label') = 'Gene B'
  nodeData (g, 'C', 'label') = 'Gene C'

  g = graph::addEdge ('A', 'B', g)
  g = graph::addEdge ('B', 'C', g)
  g = graph::addEdge ('C', 'A', g)

  edgeData (g, 'A', 'B', 'edgeType') = 'phosphorylates'
  edgeData (g, 'B', 'C', 'edgeType') = 'synthetic lethal'

  edgeData (g, 'A', 'B', 'score') =  35.0
  edgeData (g, 'B', 'C', 'score') =  -12

  return (g)

} # makeSimpleGraph
#------------------------------------------------------------------------------------------------------------------------
makeRandomGraph = function (node.count=12, seed = 123)
{
  set.seed (seed); 
  #if (node.count > 26) node.count = 26
  node.names = as.character (1:node.count)
  g = randomGraph (node.names, M <- 1:2, p = 0.6)
  attr (edgeDataDefaults (g, attr="weight"), "class") = "DOUBLE"
  edgeDataDefaults (g, 'pmid') = '9988778899'
  attr (edgeDataDefaults (g, attr="pmid"), "class") = "STRING"
  return (g)

} # makeRandomGraph
#------------------------------------------------------------------------------------------------------------------------
remove.redundancies.in.undirected.graph = function (gu)
{
  if (length (nodes (gu)) == 0)
    return (new ('graphNEL', edgemode='directed'))

  g = new ('graphNEL', edgemode='directed')

  if (length (edgeDataDefaults (gu)) > 0)
    edgeDataDefaults (g) = edgeDataDefaults (gu)

  if (length (nodeDataDefaults (gu)) > 0)
    nodeDataDefaults (g) = nodeDataDefaults (gu)

  g = addNode (nodes (gu), g)
  for (node in nodes (g)) {
    for (noa.name in noa.names (gu)) {
      nodeData (g, node, noa.name) = nodeData (gu, node, noa.name)
      } # for noa.name
    } # for node

  if (length (edges (gu)) == 0)
    return (g)

  edge.names = edgeNames (gu)
  edge.node.pairs = strsplit (edge.names, '\\~')
  eda.names = eda.names (gu)

  for (node.pair in edge.node.pairs) {
    source.node = node.pair [1]
    target.node = node.pair [2]
    #printf ('create edge from %s to %s', source.node, target.node)
    g = addEdge (source.node, target.node, g)
    for (eda.name in eda.names (gu)) {
      edgeData (g, source.node, target.node, eda.name) = edgeData (gu, source.node, target.node, eda.name)
      } # for eda.name
    } # for node.pair

  return (g)

} # remove.redundancies.in.undirected.graph 
#------------------------------------------------------------------------------------------------------------------------
.cleanup = function (libpath)
{
  cw.closer = CytoscapeWindow ('closer', create.window=FALSE)
  destroyAllWindows (cw.closer)
  
} # .onUnload
#------------------------------------------------------------------------------------------------------------------------
