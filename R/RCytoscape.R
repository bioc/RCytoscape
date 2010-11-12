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
setGeneric ('version',                  signature='obj', function (obj) standardGeneric ('version'))
setGeneric ('msg',                      signature='obj', function (obj, string) standardGeneric ('msg'))
setGeneric ('clearMsg',                 signature='obj', function (obj) standardGeneric ('clearMsg'))
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
setGeneric ('setGraph',                 signature='obj', function (obj, graph) standardGeneric ('setGraph'))
setGeneric ('getGraph',                 signature='obj', function (obj) standardGeneric ('getGraph'))
setGeneric ('sendNodes',                signature='obj', function (obj) standardGeneric ('sendNodes'))
setGeneric ('sendEdges',                signature='obj', function (obj) standardGeneric ('sendEdges'))
setGeneric ('sendNodeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('sendNodeAttributes'))
setGeneric ('sendNodeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, node.names, values) standardGeneric ('sendNodeAttributesDirect'))
setGeneric ('sendEdgeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('sendEdgeAttributes'))
setGeneric ('sendEdgeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, edge.names, values) standardGeneric ('sendEdgeAttributesDirect'))
setGeneric ('displayGraph',             signature='obj', function (obj) standardGeneric ('displayGraph'))
setGeneric ('layout',                   signature='obj', function (obj, layout.name='jgraph-spring') standardGeneric ('layout'))
setGeneric ('setPosition',              signature='obj', function (obj, node.names, x.coords, y.coords) standardGeneric ('setPosition'))
setGeneric ('redraw',                   signature='obj', function (obj) standardGeneric ('redraw'))
setGeneric ('hidePanel',                signature='obj', function (obj, panelName) standardGeneric ('hidePanel'))
setGeneric ('dockPanel',                signature='obj', function (obj, panelName) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',               signature='obj', function (obj, panelName) standardGeneric ('floatPanel'))

setGeneric ('setDefaultNodeShape',        signature='obj', function (obj, new.shape, vizmap.style.name='default') standardGeneric ('setDefaultNodeShape'))
setGeneric ('setDefaultNodeSize',         signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultNodeSize'))
setGeneric ('setDefaultNodeColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeColor'))
setGeneric ('setDefaultNodeBorderColor',  signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeBorderColor'))
setGeneric ('setDefaultNodeBorderWidth',  signature='obj', function (obj, new.width, vizmap.style.name='default') standardGeneric ('setDefaultNodeBorderWidth'))
setGeneric ('setDefaultNodeFontSize',     signature='obj', function (obj, new.size, vizmap.style.name='default') standardGeneric ('setDefaultNodeFontSize'))
setGeneric ('setDefaultNodeLabelColor',   signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeLabelColor'))

setGeneric ('setDefaultEdgeLineWidth',    signature='obj', function (obj, new.width, vizmap.style.name='default') standardGeneric ('setDefaultEdgeLineWidth'))
setGeneric ('setDefaultEdgeColor',        signature='obj', function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeColor'))


setGeneric ('setNodeTooltipRule',       signature='obj', function (obj, node.attribute.name) standardGeneric ('setNodeTooltipRule'))
setGeneric ('setEdgeTooltipRule',       signature='obj', function (obj, edge.attribute.name) standardGeneric ('setEdgeTooltipRule'))
setGeneric ('setNodeLabelRule',         signature='obj', function (obj, node.attribute.name) standardGeneric ('setNodeLabelRule'))
setGeneric ('setNodeColorRule',         signature='obj', 
    function (obj, node.attribute.name, control.points, colors, mode='interpolate', default.color='#FFFFFF') standardGeneric ('setNodeColorRule'))

setGeneric ('setNodeBorderColorRule',   signature='obj', 
    function (obj, node.attribute.name, control.points, colors, mode='interpolate', default.color='#000000') standardGeneric ('setNodeBorderColorRule'))

setGeneric ('setNodeShapeRule',         signature='obj', 
    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') standardGeneric ('setNodeShapeRule'))
setGeneric ('setNodeSizeRule',          signature='obj', 
    function (obj, node.attribute.name, control.points, node.sizes, mode='interpolate', default.size=40) standardGeneric ('setNodeSizeRule'))
#setGeneric ('setBorderWidth',           signature='obj', function (obj, new.size) standardGeneric ('setNodeBorderWidth'))
setGeneric ('setEdgeLineStyleRule',     signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') standardGeneric ('setEdgeLineStyleRule'))
setGeneric ('setEdgeTargetArrowRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') standardGeneric ('setEdgeTargetArrowRule'))
setGeneric ('setEdgeSourceArrowRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') standardGeneric ('setEdgeSourceArrowRule'))

setGeneric ('setEdgeTargetArrowColorRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') standardGeneric ('setEdgeTargetArrowColorRule'))
setGeneric ('setEdgeSourceArrowColorRule',   signature='obj', 
    function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') standardGeneric ('setEdgeSourceArrowColorRule'))

setGeneric ('setEdgeColorRule',         signature='obj',
    function (obj, attribute.name, attribute.values, colors, default.color='#000000') standardGeneric ('setEdgeColorRule'))

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

  function (object) {
    if (length (object@title) != 1) 
      "'title' is not a single string" 
    else if (!nzchar (object@title))
      "'title' is an empty string" 
    validObject (object@graph)
    })

#------------------------------------------------------------------------------------------------------------------------
# the class constructor, defined as a simple function, with no formal link to the class
CytoscapeWindow = function (title='default', graph=new('graphNEL', edgemode='directed'), host='localhost', rpcPort=9000, create.window=TRUE)
{
  
  # this code is for the Bioconductor build system. You should never need to set or
  # read these environment variables in ordinary use.
  if ((Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE") != "")) {
    host = Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE")
    rpcPort = as(Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE"),"integer")
  }
  
  uri = sprintf ('http://%s:%s', host, rpcPort)

    # add a label to each node if not already present.  default label is the node name, the node ID
  if (edgemode (graph) == 'undirected') 
    graph = remove.redundancies.in.undirected.graph (graph)

  if (! 'label' %in% noa.names (graph)) {
    write ('nodes have no label attribute -- adding default labels', stderr ())
    graph = initNodeAttribute (graph, 'label', 'char', '')

    #nodeDataDefaults (graph, 'label') = ''
    #attr (nodeDataDefaults (graph, attr = "label"), "class") = "STRING"

    if (length (nodes (graph) > 0))
      for (node in nodes (graph))
        nodeData (graph, node, 'label') = node
    } # if no label node attribute

  cw = new ('CytoscapeWindowClass', title=title, graph=graph, uri=uri)
  plugin.version.string = version (cw)
  plugin.version = as.numeric ((strsplit (plugin.version.string,' ')[[1]][1]))
  
  expected.version = 1.2
  if (plugin.version < expected.version) { 
    write (' ', stderr ())
    write (sprintf ('This version of the RCytoscape package requires CytoscapeRPC plugin version %s or greater.', expected.version), stderr ())
    write (sprintf ('However, you are using version %s.   You must upgrade.', plugin.version), stderr ())
    write ('Please visit the plugins page at http://www.cytoscape.org.', stderr ())
    write (' ', stderr ())
    stop ('Wrong CytoscapeRPC version.')
    }
    
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
setMethod ('version', 'CytoscapeWindowClass', 
  function (obj) { 
    return (xml.rpc (obj@uri, 'Cytoscape.version'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('msg', 'CytoscapeWindowClass', 
  function (obj, string) { 
    invisible (xml.rpc (obj@uri, 'Cytoscape.setStatusBarMessage', string))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('clearMsg', 'CytoscapeWindowClass', 
  function (obj) { 
    invisible (xml.rpc (obj@uri, 'Cytoscape.clearStatusBarMessage'))
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
setMethod ('setGraph', 'CytoscapeWindowClass',

  function (obj, graph) {
    if (edgemode (graph) == 'undirected') 
      graph = remove.redundancies.in.undirected.graph (graph)
    obj@graph = graph
    return (obj)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getGraph', 'CytoscapeWindowClass',

  function (obj) {
    return (obj@graph)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendNodes', 'CytoscapeWindowClass',

  function (obj) {

     if (length (nodes (obj@graph)) == 0) {
       write ('CytoscapeWindow.sendNodes, no nodes in graph.  returning', stderr ())
       return ()
       }

     invisible (xml.rpc (obj@uri, 'Cytoscape.createNodes', as.character (obj@window.id), nodes (obj@graph)))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdges', 'CytoscapeWindowClass',

  function (obj) {

    if (length (edgeNames (obj@graph)) == 0) {
      write ('CytoscapeWindow.sendNodes, no edges in graph.  returning', stderr ())
      return ()
      }

    tokens = strsplit (edgeNames (obj@graph), '~')
    a = sapply (tokens, function (tok) tok [1])
    b = sapply (tokens, function (tok) tok [2])
    edge.type = as.character (eda (obj@graph, 'edgeType'))
    if (length (edge.type) == 1 && is.na (edge.type))
      edge.type = rep ('unspecified', length (tokens))
    directed = rep (TRUE, length (tokens))
    forgive.if.node.is.missing = TRUE
    xml.rpc (obj@uri, 'Cytoscape.createEdges', as.character (obj@window.id), a, b, edge.type, directed, forgive.if.node.is.missing, .convert=F)
    }) # sendEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod ('layout', 'CytoscapeWindowClass',

  function (obj, layout.name='jgraph-spring') {

    if (!layout.name %in% getLayoutNames (obj)) {
      write (sprintf ("layout '%s' is not recognized; call getLayoutNames (<CytoscapeWindow>) to see those which are supported", layout.name), stderr ())
      return ()
      }

    id = as.character (obj@window.id)
    invisible (xml.rpc (obj@uri, 'Cytoscape.performLayout', id, layout.name))
    }) # cy.layout

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setPosition', 'CytoscapeWindowClass',

  function (obj, node.names, x.coords, y.coords) {

    count = length (node.names)
    stopifnot (length (x.coords) == count)
    stopifnot (length (y.coords) == count)

    if (count == 0)
      return ()

    id = as.character (obj@window.id)

    if (count == 1)
      invisible (xml.rpc (obj@uri, 'Cytoscape.setNodePosition', id, node.names, x.coords, y.coords))
    else 
      invisible (xml.rpc (obj@uri, 'Cytoscape.setNodesPositions', id, node.names, x.coords, y.coords))

    }) # cy.setPosition

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
     invisible (sendNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, node.names, values))
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

     invisible (result)
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

     invisible (sendEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, edge.names, values))
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

     invisible (result)
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
     invisible (xml.rpc (obj@uri, 'Cytoscape.redraw', id))
     }) # redraw

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hidePanel', 'CytoscapeWindowClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.hidePanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('dockPanel', 'CytoscapeWindowClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.dockPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('floatPanel', 'CytoscapeWindowClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.floatPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeTooltipRule', 'CytoscapeWindowClass',

  function (obj, node.attribute.name) {
    id = as.character (obj@window.id)
    viz.style.name = 'default'
    attribute.values = as.character (noa (obj@graph, node.attribute.name))
    tooltips = attribute.values
    xml.rpc (obj@uri, 'Cytoscape.discreteMapper', id, viz.style.name, node.attribute.name, 'Node Tooltip', '', 
             attribute.values, tooltips)
    })  # setNodeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTooltipRule', 'CytoscapeWindowClass',

  function (obj, edge.attribute.name) {
    id = as.character (obj@window.id)
    viz.style.name = 'default'
    attribute.values = as.character (eda (obj@graph, edge.attribute.name))
    tooltips = attribute.values
    xml.rpc (obj@uri, 'Cytoscape.discreteMapper', id, viz.style.name, edge.attribute.name, 'Edge Tooltip', '', 
             attribute.values, tooltips)
    })  # setEdgeTooltipRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeLabelRule', 'CytoscapeWindowClass',

  function (obj, node.attribute.name) {
    id = as.character (obj@window.id)
    xml.rpc (obj@uri, 'Cytoscape.setNodeLabel', id, node.attribute.name, 'label', 'default'); 
    redraw (obj)
    })  # setNodeLabelRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeColorRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, colors, mode='interpolate', default.color='#FFFFFF') {

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultNodeColor (obj, default.color)
     if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
       if (length (control.points) == length (colors)) { # called did not supply 'below' and 'above' values; manufacture them
         colors = c (colors [1], colors, colors [length (colors)])
         #write ("RCytoscape::setNodeColorRule, no 'below' or 'above' colors specified.  Inferred from supplied colors.", stderr ());
         } # 

       good.args = length (control.points) == (length (colors) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeColorRule, interpolate mode.", stderr ())
         write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Color', control.points, colors)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (colors)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', as.character (obj@window.id), default.style, 
                         node.attribute.name, 'Node Color', default.color, control.points, colors)
       invisible (result)
       } # else: !interpolate
     }) # setNodeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderColorRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, control.points, colors, mode='interpolate', default.color='#000000') {

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeBorderColorRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultNodeBorderColor (obj, default.color)
     
     if (mode=='interpolate') {  # need a 'below' color and an 'above' color.  so there should be two more colors than control.points 
       if (length (control.points) == length (colors))  # called did not supply 'below' and 'above' values; manufacture them
         colors = c (default.color, colors, default.color)

       good.args = length (control.points) == (length (colors) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeBorderColorRule, interpolate mode.", stderr ())
         write ("Expecting 1 color for each control.point, one for 'above' color, one for 'below' color.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Border Color', control.points, colors)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (colors)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (colors)), stderr ())
         write ("Error! RCytoscape:setNodeBorderColorRule.  Expecting exactly as many colors as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', as.character (obj@window.id), default.style, 
                         node.attribute.name, 'Node Border Color', default.color, control.points, colors)
       invisible (result)
       } # else: !interpolate
     }) # setNodeBorderColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeShape', 'CytoscapeWindowClass', 

   function (obj, new.shape, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Shape', new.shape); 
     #redraw (obj)
     }) # setDefaultNodeShape

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeSize', 'CytoscapeWindowClass', 

   function (obj, new.size, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Size', as.character (new.size)); 
     #redraw (obj)
     }) # setDefaultNodeSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeColor', 'CytoscapeWindowClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Color', as.character (new.color)); 
     }) # setDefaultNodeColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderColor', 'CytoscapeWindowClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Border Color', as.character (new.color)); 
     }) # setDefaultNodeBorderColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeBorderWidth', 'CytoscapeWindowClass', 

   function (obj, new.width, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Line Width', as.character (new.width)); 
     }) # setDefaultNodeBorderWidth

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeFontSize', 'CytoscapeWindowClass', 

   function (obj, new.size, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Font Size', as.character (new.size));
     }) # setDefaultNodeFontSize

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeLabelColor', 'CytoscapeWindowClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Node Label Color', as.character (new.color)); 
     }) # setDefaultNodeLabelColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeLineWidth', 'CytoscapeWindowClass', 

   function (obj, new.width, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Edge Line Width', as.character (new.width)); 
     }) # setDefaultEdgeLineWidth

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeColor', 'CytoscapeWindowClass', 

   function (obj, new.color, vizmap.style.name='default') {
     xml.rpc (obj@uri, 'Cytoscape.setDefaultVizMapValue', 'default', 'Edge Color', as.character (new.color)); 
     }) # setDefaultEdgeColor

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeShapeRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') {
     setDefaultNodeShape (obj, default.shape)
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setNodeShapeRule", id, node.attribute.name, default.shape, 
                      attribute.values, node.shapes, .convert=TRUE)
     invisible (result)
     }) # setNodeShapeRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeSizeRule', 'CytoscapeWindowClass',

#   function (obj, node.attribute.name, attribute.values, node.sizes) {
#     id = as.character (obj@window.id)
#        # take the first and last node size, prepend and append them respectively, so that there are 2 more
#        # visual attribute values (in this case, node size in pixels) than there are node data attribute values
#     adjusted.node.sizes = c (node.sizes [1], node.sizes, c (node.sizes [length (node.sizes)]))
#     result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Size', 
#                       attribute.values, adjusted.node.sizes)
#     return (result)
#     }) # setNodeSizeRule

   function (obj, node.attribute.name, control.points, node.sizes, mode='interpolate', default.size=40) {

     if (!mode %in% c ('interpolate', 'lookup')) {
       write ("Error! RCytoscape:setNodeSizeRule.  mode must be 'interpolate' (the default) or 'lookup'.", stderr ())
       return ()
       }

     setDefaultNodeSize (obj, default.size)

     if (mode=='interpolate') {  # need a 'below' size and an 'above' size.  so there should be two more colors than control.points 
       if (length (control.points) == length (node.sizes)) { # caller did not supply 'below' and 'above' values; manufacture them
         node.sizes = c (node.sizes [1], node.sizes, node.sizes [length (node.sizes)])
         write ("RCytoscape::setNodeSizeRule, no 'below' or 'above' sizes specified.  Inferred from node.sizes.", stderr ())
         } # 

       good.args = length (control.points) == (length (node.sizes) - 2)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (node.sizes)), stderr ())
         write ("Error! RCytoscape:setNodeSizeRule, interpolate mode.", stderr ())
         write ("Expecting 1 node.size for each control.point, one for 'above' size, one for 'below' size.", stderr ())
         return ()
         }
       result = xml.rpc (obj@uri, 'Cytoscape.createContinuousNodeVisualStyle', node.attribute.name, 'Node Size', control.points, node.sizes)
       invisible (result)
       } # if mode==interpolate

     else { # use a discrete rule, with no interpolation
       good.args = length (control.points) == length (node.sizes)
       if (!good.args) {
         write (sprintf ('cp: %d', length (control.points)), stderr ())
         write (sprintf ('co: %d', length (node.sizes)), stderr ())
         write ("Error! RCytoscape:setNodeSizeRule.  Expecting exactly as many node.sizes as control.points in lookup mode.", stderr ())
         return ()
         }

       default.style = 'default'
       result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', as.character (obj@window.id), default.style, 
                         node.attribute.name, 'Node Size', as.character (default.size), 
                         as.character (control.points), as.character (node.sizes))
       invisible (result)
       } # else: !interpolate
     }) # setNodeSizeRule


#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorRule', 'CytoscapeWindowClass',

 function (obj, attribute.name, attribute.values, colors, default.color='#000000') {
     setDefaultEdgeColor (obj, default.color)
     id = as.character (obj@window.id)
     default.color = '#000000'
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeColorRule", id, attribute.name, default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeLineStyleRule", id, edge.attribute.name, default.style, 
                       attribute.values, line.styles, .convert=TRUE)
     invisible (result)
     }) # set.edge.line.style.rule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeTargetArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeSourceArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
     id = as.character (obj@window.id)
     style.name = 'default'
     result = xml.rpc (obj@uri, "Cytoscape.discreteMapper", id, style.name, edge.attribute.name,
                      'Edge Target Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
     id = as.character (obj@window.id)
     style.name = 'default'
     result = xml.rpc (obj@uri, "Cytoscape.discreteMapper", id, style.name, edge.attribute.name,
                      'Edge Source Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countNodes", id)
     if (count == 0)
       return ()
       # todo:  getting all nodes should be inherently a window-specific operation
     result = xml.rpc (obj@uri, "Cytoscape.getNodes", id, .convert=TRUE)
     #result = xml.rpc (obj@uri, "Cytoscape.getAllNodes", .convert=TRUE)
     return (result)
     }) # getAllNodes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAllEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countEdges", id)
     if (count == 0)
       return ()
     result = xml.rpc (obj@uri, "Cytoscape.getEdges", id, .convert=TRUE)
     #result = xml.rpc (obj@uri, "Cytoscape.getAllEdges", .convert=TRUE)
     return (result)
     }) # getAllEdges

#------------------------------------------------------------------------------------------------------------------------
setMethod ('selectNodes', 'CytoscapeWindowClass',

   function (obj, node.names) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.selectNodes',id, node.names, .convert=TRUE)
     redraw (obj)
     invisible (result)
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
     invisible (result)
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
     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedNodes', id, .convert=TRUE))
     }) # hideSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('unhideAll', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.unhideAll', id, .convert=TRUE))
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
  if (!edge.attribute.name %in% eda.names (graph))
    return (NA)

  return (unlist (edgeData (graph, attr=edge.attribute.name)))

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

  g = initNodeAttribute (g, 'type', 'char', 'undefined')
  g = initNodeAttribute (g, 'lfc', 'numeric', 1.0)
  g = initNodeAttribute (g, 'label', 'char', 'default node label')
  g = initNodeAttribute (g, 'count', 'integer', 0)

  g = initEdgeAttribute (g, 'edgeType', 'char', 'undefined')
  g = initEdgeAttribute (g, 'score', 'numeric', 0.0)
  g = initEdgeAttribute (g, 'misc',   'char', 'default misc')

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
  g = initEdgeAttribute (g, 'edgeType', 'char', 'random')
  g = initEdgeAttribute (g, 'weight', 'numeric', 0.0)
  g = initEdgeAttribute (g, 'pmid',   'char', '9988778899')

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
initNodeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
  stopifnot (attribute.type %in% c ('char', 'integer', 'numeric'))
  if (attribute.type == 'char')
    attribute.type = 'STRING'
  else if (attribute.type == 'integer')
    attribute.type = 'INTEGER'
  else if (attribute.type == 'numeric')
    attribute.type = 'FLOATING'

  nodeDataDefaults (graph, attr=attribute.name) = default.value
  attr (nodeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type

  return (graph)

} # initNodeAttribute
#------------------------------------------------------------------------------------------------------------------------
initEdgeAttribute = function (graph, attribute.name, attribute.type, default.value)
{
  stopifnot (attribute.type %in% c ('char', 'integer', 'numeric'))
  if (attribute.type == 'char')
    attribute.type = 'STRING'
  else if (attribute.type == 'integer')
    attribute.type = 'INTEGER'
  else if (attribute.type == 'numeric')
    attribute.type = 'FLOATING'

  edgeDataDefaults (graph, attr=attribute.name) = default.value
  attr (edgeDataDefaults (graph, attr=attribute.name), 'class') = attribute.type

  return (graph)

} # initEdgettribute
#------------------------------------------------------------------------------------------------------------------------
.cleanup = function (libpath)
{
  cw.closer = CytoscapeWindow ('closer', create.window=FALSE)
  destroyAllWindows (cw.closer)
  
} # .onUnload
#------------------------------------------------------------------------------------------------------------------------
