library (graph)
library (XMLRPC)
library (methods)
#------------------------------------------------------------------------------------------------------------------------
printf = function (...) print (noquote (sprintf (...)))
#------------------------------------------------------------------------------------------------------------------------
setClass ("CytoscapeConnectionClass", 
          representation = representation (uri="character"),
          prototype = prototype (uri="http://localhost:9000")
          )

#------------------------------------------------------------------------------------------------------------------------
setClass ("CytoscapeWindowClass", 
          representation = representation (title="character",
                                           window.id='character',
                                           graph="graph"),
          contains='CytoscapeConnectionClass',
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
setGeneric ('destroyWindow',            signature='obj', function (obj, windowTitle) standardGeneric ('destroyWindow'))
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

setGeneric ('addNodes',                 signature='obj', function (obj, other.graph) standardGeneric ('addNodes'))
setGeneric ('addEdges',                 signature='obj', function (obj, other.graph) standardGeneric ('addEdges'))
setGeneric ('addGraphToGraph',          signature='obj', function (obj, other.graph) standardGeneric ('addGraphToGraph'))
setGeneric ('sendNodeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('sendNodeAttributes'))
setGeneric ('sendNodeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, node.names, values) standardGeneric ('sendNodeAttributesDirect'))
setGeneric ('sendEdgeAttributes',       signature='obj', function (obj, attribute.name) standardGeneric ('sendEdgeAttributes'))
setGeneric ('sendEdgeAttributesDirect', signature='obj', 
    function (obj, attribute.name, attribute.type, edge.names, values) standardGeneric ('sendEdgeAttributesDirect'))
setGeneric ('displayGraph',             signature='obj', function (obj) standardGeneric ('displayGraph'))
setGeneric ('layout',                   signature='obj', function (obj, layout.name='jgraph-spring') standardGeneric ('layout'))
setGeneric ('setPosition',              signature='obj', function (obj, node.names, x.coords, y.coords) standardGeneric ('setPosition'))
setGeneric ('getPosition',              signature='obj', function (obj, node.names) standardGeneric ('getPosition'))
setGeneric ('redraw',                   signature='obj', function (obj) standardGeneric ('redraw'))
setGeneric ('hidePanel',                signature='obj', function (obj, panelName) standardGeneric ('hidePanel'))
setGeneric ('hideAllPanels',            signature='obj', function (obj) standardGeneric ('hideAllPanels'))
setGeneric ('dockPanel',                signature='obj', function (obj, panelName) standardGeneric ('dockPanel'))
setGeneric ('floatPanel',               signature='obj', function (obj, panelName) standardGeneric ('floatPanel'))

setGeneric ('getDefaultBackgroundColor',  signature='obj', 
             function (obj, vizmap.style.name='default') standardGeneric ('getDefaultBackgroundColor'))
setGeneric ('setDefaultBackgroundColor',  signature='obj', 
             function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultBackgroundColor'))

setGeneric ('getDefaultNodeSelectionColor',  signature='obj', 
             function (obj, vizmap.style.name='default') standardGeneric ('getDefaultNodeSelectionColor'))
setGeneric ('setDefaultNodeSelectionColor',  signature='obj', 
             function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeSelectionColor'))

setGeneric ('getDefaultNodeReverseSelectionColor',  signature='obj',
                function (obj, vizmap.style.name='default') standardGeneric ('getDefaultNodeReverseSelectionColor'))
setGeneric ('setDefaultNodeReverseSelectionColor',  signature='obj',
                function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultNodeReverseSelectionColor'))

setGeneric ('getDefaultEdgeSelectionColor',  signature='obj', 
                function (obj, vizmap.style.name='default') standardGeneric ('getDefaultEdgeSelectionColor'))
setGeneric ('setDefaultEdgeSelectionColor',  signature='obj', 
             function (obj, new.color,  vizmap.style.name='default') standardGeneric ('setDefaultEdgeSelectionColor'))

setGeneric ('getDefaultEdgeReverseSelectionColor',  signature='obj',
                function (obj, vizmap.style.name='default') standardGeneric ('getDefaultEdgeReverseSelectionColor'))
setGeneric ('setDefaultEdgeReverseSelectionColor',  signature='obj',
                function (obj, new.color, vizmap.style.name='default') standardGeneric ('setDefaultEdgeReverseSelectionColor'))

setGeneric ('saveImage',                  signature='obj', function (obj, file.name, image.type, scale) standardGeneric ('saveImage'))

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
setGeneric ('setEdgeLabelRule',         signature='obj', function (obj, edge.attribute.name) standardGeneric ('setEdgeLabelRule'))

setGeneric ('setNodeColorRule',         signature='obj', 
    function (obj, node.attribute.name, control.points, colors, mode='interpolate', default.color='#FFFFFF') standardGeneric ('setNodeColorRule'))

setGeneric ('setNodeBorderColorRule',   signature='obj', 
    function (obj, node.attribute.name, control.points, colors, mode='interpolate', default.color='#000000') standardGeneric ('setNodeBorderColorRule'))

setGeneric ('setNodeBorderWidthRule',   signature='obj', 
    function (obj, node.attribute.name, attribute.values, line.widths, default.width=1) standardGeneric ('setNodeBorderWidthRule'))

setGeneric ('setNodeShapeRule',         signature='obj', 
    function (obj, node.attribute.name, attribute.values, node.shapes, default.shape='ellipse') standardGeneric ('setNodeShapeRule'))
setGeneric ('setNodeSizeRule',          signature='obj', 
    function (obj, node.attribute.name, control.points, node.sizes, mode='interpolate', default.size=40) standardGeneric ('setNodeSizeRule'))

setGeneric ('setEdgeLineStyleRule',     signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') standardGeneric ('setEdgeLineStyleRule'))

setGeneric ('setEdgeLineWidthRule', signature='obj', 
    function (obj, edge.attribute.name, attribute.values, line.widths, default.width='1') standardGeneric ('setEdgeLineWidthRule'))

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

setGeneric ('getNodeCount',             signature='obj', function (obj) standardGeneric ('getNodeCount'))
setGeneric ('getEdgeCount',             signature='obj', function (obj) standardGeneric ('getEdgeCount'))
setGeneric ('getNodeAttribute',         signature='obj', function (obj, node.name, attribute.name) standardGeneric ('getNodeAttribute'))
setGeneric ('getEdgeAttribute',         signature='obj', function (obj, edge.name, attribute.name) standardGeneric ('getEdgeAttribute'))
setGeneric ('getNodeAttributeNames',    signature='obj', function (obj) standardGeneric ('getNodeAttributeNames'))
setGeneric ('getEdgeAttributeNames',    signature='obj', function (obj) standardGeneric ('getEdgeAttributeNames'))
setGeneric ('deleteNodeAttribute',      signature='obj', function (obj, attribute.name) standardGeneric ('deleteNodeAttribute'))
setGeneric ('deleteEdgeAttribute',      signature='obj', function (obj, attribute.name) standardGeneric ('deleteEdgeAttribute'))
setGeneric ('getAllNodes',              signature='obj', function (obj) standardGeneric ('getAllNodes'))
setGeneric ('getAllEdges',              signature='obj', function (obj) standardGeneric ('getAllEdges'))
setGeneric ('selectNodes',              signature='obj', function (obj, node.names) standardGeneric ('selectNodes'))
setGeneric ('getSelectedNodes',         signature='obj', function (obj) standardGeneric ('getSelectedNodes'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedNodeCount',     signature='obj', function (obj) standardGeneric ('getSelectedNodeCount'))
setGeneric ('hideSelectedNodes',        signature='obj', function (obj) standardGeneric ('hideSelectedNodes'))
setGeneric ('invertNodeSelection',      signature='obj', function (obj) standardGeneric ('invertNodeSelection'))
setGeneric ('removeSelectedNodes',      signature='obj', function (obj, remove.from.root.graph.also=TRUE) standardGeneric ('removeSelectedNodes'))

#setGeneric ('selectEdges',             signature='obj', function (obj, edge.names) standardGeneric ('selectEdges'))
setGeneric ('invertEdgeSelection',      signature='obj', function (obj) standardGeneric ('invertEdgeSelection'))
setGeneric ('removeSelectedEdges',      signature='obj', function (obj, remove.from.root.graph.also=TRUE) standardGeneric ('removeSelectedEdges'))

setGeneric ('getSelectedEdges',         signature='obj', function (obj) standardGeneric ('getSelectedEdges'))
setGeneric ('clearSelection',           signature='obj', function (obj) standardGeneric ('clearSelection'))
setGeneric ('getSelectedEdgeCount',     signature='obj', function (obj) standardGeneric ('getSelectedEdgeCount'))
setGeneric ('hideSelectedEdges',        signature='obj', function (obj) standardGeneric ('hideSelectedEdges'))

setGeneric ('unhideAll',                signature='obj', function (obj) standardGeneric ('unhideAll'))

setGeneric ('firstNeighbors',           signature='obj', function (obj, nodeName) standardGeneric ('firstNeighbors'))
setGeneric ('cy.sfn',                   signature='obj', function (obj) standardGeneric ('cy.sfn'))
#-----------------------------------------------------------
# methods related to transmitting data from Cytoscape to R
#-----------------------------------------------------------
setGeneric ('getWindowID',                   signature='obj', function (obj, window.title) standardGeneric ('getWindowID'))
setGeneric ('haveNodeAttribute',             signature='obj', function (obj, node.names, attribute.name) standardGeneric ('haveNodeAttribute'))
setGeneric ('haveEdgeAttribute',             signature='obj', function (obj, edge.names, attribute.name) standardGeneric ('haveEdgeAttribute'))
setGeneric ('copyNodeAttributesFromCyGraph', signature='obj', function (obj, window.id, existing.graph) standardGeneric ('copyNodeAttributesFromCyGraph'))
setGeneric ('copyEdgeAttributesFromCyGraph', signature='obj', function (obj, window.id, existing.graph) standardGeneric ('copyEdgeAttributesFromCyGraph'))
setGeneric ('getGraphFromCyWindow',          signature='obj', function (obj, window.title) standardGeneric ('getGraphFromCyWindow'))

#-----------------------------------------------------------
# methods related to visual styles
#-----------------------------------------------------------
setGeneric ('getVisualStyleNames', signature='obj', function (obj) standardGeneric ('getVisualStyleNames'))
setGeneric ('copyVisualStyle',     signature='obj', function (obj, from.style, to.style) standardGeneric ('copyVisualStyle'))
setGeneric ('setVisualStyle',      signature='obj', function (obj, new.style.name) standardGeneric ('setVisualStyle'))
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
CytoscapeConnection = function (host='localhost', rpcPort=9000)
{
  
  # this code is for the Bioconductor build system. You should never need to set or
  # read these environment variables in ordinary use.
  if ((Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE") != "")) {
    host = Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE")
    rpcPort = as(Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE"),"integer")
    }
  
  
  uri = sprintf ('http://%s:%s', host, rpcPort)
  cc = new ('CytoscapeConnectionClass', uri=uri)
  return (cc)

} # CytoscapeConnection
#------------------------------------------------------------------------------------------------------------------------
# the 'new window' class constructor, defined as a simple function, with no formal link to the class
new.CytoscapeWindow = function (title, graph=new('graphNEL', edgemode='directed'), host='localhost', rpcPort=9000, create.window=TRUE)
{
  
  # this code is for the Bioconductor build system. You should never need to set or
  # read these environment variables in ordinary use.
  if ((Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE") != "")) {
    host = Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE")
    rpcPort = as(Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE"),"integer")
    }
  
  uri = sprintf ('http://%s:%s', host, rpcPort)

  cy.tmp = CytoscapeConnection (host, rpcPort)
  check.cytoscape.plugin.version (cy.tmp)

  if (!is.na (getWindowID (cy.tmp, title))) {
    write (sprintf ('There is already a window in Cytoscape named "%s".  Please use a unique name.', title), stderr ())
    stop ()
    }

    # add a label to each node if not already present.  default label is the node name, the node ID
  if (edgemode (graph) == 'undirected') 
    graph = remove.redundancies.in.undirected.graph (graph)

  if (! 'label' %in% noa.names (graph)) {
    #write ('nodes have no label attribute -- adding default labels', stderr ())
    graph = initNodeAttribute (graph, 'label', 'char', '')


    if (length (nodes (graph) > 0))
      for (node in nodes (graph))
        nodeData (graph, node, 'label') = node
    } # if no label node attribute

  cw = new ('CytoscapeWindowClass', title=title, graph=graph, uri=uri)

  if (create.window)
    cw@window.id = createWindow (cw)

  return (cw)

} # new.CytsoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
CytoscapeWindow = new.CytoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
# the 'existing window' class constructor, defined as a simple function, with no formal link to the class
existing.CytoscapeWindow = function (title, host='localhost', rpcPort=9000, copy.graph.from.cytoscape.to.R=FALSE)
{
  
  # this code is for the Bioconductor build system. You should never need to set or
  # read these environment variables in ordinary use.
  if ((Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE") != "") &&  (Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE") != "")) {
    host = Sys.getenv("RCYTOSCAPE_HOST_OVERRIDE")
    rpcPort = as(Sys.getenv("RCYTOSCAPE_PORT_OVERRIDE"),"integer")
    }
  
  uri = sprintf ('http://%s:%s', host, rpcPort)

  cy.tmp = CytoscapeConnection (host, rpcPort)     # create this (inexpensively) just to gain access tothe window list
  check.cytoscape.plugin.version (cy.tmp)

  existing.window.id = getWindowID (cy.tmp, title)

  if (is.na (existing.window.id)) {
    write (sprintf ('There is no window in Cytoscape named "%s".  Please choose from the following titles:.', title), stderr ())
    write (as.character (getWindowList (cy.tmp)), stderr ())
    return (NA)
    }

  cw = new ('CytoscapeWindowClass', title=title, window.id=existing.window.id, uri=uri)

  if (copy.graph.from.cytoscape.to.R) {
    g.cy = getGraphFromCyWindow (cw, title)
    cw = setGraph (cw, g.cy)
    }

  return (cw)

} # existing.CytsoscapeWindow
#------------------------------------------------------------------------------------------------------------------------
check.cytoscape.plugin.version = function (cyCon)
{
  plugin.version.string = version (cyCon)
  string.tmp1 = strsplit (plugin.version.string,' ')[[1]][1]
  string.tmp2 = gsub ('[a-z]', '', string.tmp1)
  string.tmp3 = gsub ('[A-Z]', '', string.tmp2)
  plugin.version = as.numeric (string.tmp3)
  # plugin.version = as.numeric ((strsplit (plugin.version.string,' ')[[1]][1]))
  
  expected.version = 1.3
  if (plugin.version < expected.version) { 
    write (' ', stderr ())
    write (sprintf ('This version of the RCytoscape package requires CytoscapeRPC plugin version %s or greater.', expected.version), stderr ())
    write (sprintf ('However, you are using version %s.   You must upgrade.', plugin.version), stderr ())
    write ('Please visit the plugins page at http://www.cytoscape.org.', stderr ())
    write (' ', stderr ())
    stop ('Wrong CytoscapeRPC version.')
    }

} # check.cytoscape.plugin.version
#------------------------------------------------------------------------------------------------------------------------
setMethod ('ping', signature = 'CytoscapeConnectionClass',
  function (obj) { 
    return (xml.rpc (obj@uri, 'Cytoscape.test'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('version', 'CytoscapeConnectionClass', 
  function (obj) { 
    return (xml.rpc (obj@uri, 'Cytoscape.version'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('msg', 'CytoscapeConnectionClass', 
  function (obj, string) { 
    invisible (xml.rpc (obj@uri, 'Cytoscape.setStatusBarMessage', string))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('clearMsg', 'CytoscapeConnectionClass', 
  function (obj) { 
    invisible (xml.rpc (obj@uri, 'Cytoscape.clearStatusBarMessage'))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('createWindow', 'CytoscapeWindowClass',
  function (obj) {
       # window.ids are often character versions of integers.  but they can be character titles, as when an SBML
       # file is imported from disk.
    window.id = xml.rpc (obj@uri, 'Cytoscape.createNetwork', obj@title, .convert=TRUE)
    #write (sprintf ('createWindow, id = %d', window.id), stderr ()) 
    return (window.id)
  })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowCount', 'CytoscapeConnectionClass',
  function (obj) {
    return (as.integer (xml.rpc (obj@uri, 'Cytoscape.getNetworkCount')))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowID', 'CytoscapeConnectionClass',
  function (obj, window.title) {
    current.window.list = getWindowList (obj)
     if (!window.title %in% as.character (current.window.list)) {
       #write (sprintf ("No existing Cytoscape window named '%s'", window.title), stderr ())
       return (NA)
       } # if unrecognized window.title

    window.entry = which (as.character (current.window.list) == window.title)
    window.id =  as.character (names (current.window.list) [window.entry])
    return (window.id)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowCount', 'CytoscapeConnectionClass',
  function (obj) {
    return (as.integer (xml.rpc (obj@uri, 'Cytoscape.getNetworkCount')))
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getWindowList', 'CytoscapeConnectionClass',

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
setMethod ('destroyWindow',  'CytoscapeConnectionClass',

  function (obj, windowTitle) {
    id = getWindowID (obj, windowTitle)
    if (is.na (id)) {
      write (sprintf ('no CytoscapeWindow with title "%s"', windowTitle), stderr ())
      return ()
      }
    xml.rpc (obj@uri, 'Cytoscape.destroyNetwork', id)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('destroyAllWindows',  'CytoscapeConnectionClass',

  function (obj) {
    ids = names (getWindowList (obj))
    invisible (sapply (ids, function (id)  xml.rpc (obj@uri, 'Cytoscape.destroyNetwork', id)))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeShapes', 'CytoscapeConnectionClass',

  function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getNodeShapeNames'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getAttributeClassNames', 'CytoscapeConnectionClass',

  function (obj) {
     return (c ('floating|numeric|double', 'integer|int', 'string|char|character'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLineStyles', 'CytoscapeConnectionClass',

  function (obj) {
    return (xml.rpc (obj@uri, 'Cytoscape.getLineStyleNames'))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getArrowShapes', 'CytoscapeConnectionClass',

   function (obj) {
     return (xml.rpc (obj@uri, 'Cytoscape.getArrowShapeNames'))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getLayoutNames', 'CytoscapeConnectionClass', 

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
# in Cytoscape, node attributes are administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveNodeAttribute', 'CytoscapeConnectionClass',

  function (obj, node.names, attribute.name) {
    indices.of.nodes.with.attribute.value = which (xml.rpc (obj@uri, 'Cytoscape.nodesHaveAttribute', attribute.name, node.names))
    if (length (indices.of.nodes.with.attribute.value) > 0)
      return (node.names [indices.of.nodes.with.attribute.value])
    else 
      return (character(0))
    })

#------------------------------------------------------------------------------------------------------------------------
# in Cytoscape, node attributes administered on a global level.  In addition, and in contrast to R, not all nodes in a graph
# will have a specific attribute define on it.  (In R, every node has every attribute)
# this function returns a list of nodes for which the specified attribute has a value in the corresponding Cytoscape network

setMethod ('haveEdgeAttribute', 'CytoscapeConnectionClass',

  function (obj, edge.names, attribute.name) {
    indices.of.edges.with.attribute.value = which (xml.rpc (obj@uri, 'Cytoscape.edgesHaveAttribute', attribute.name, edge.names))
    if (length (indices.of.edges.with.attribute.value) > 0)
      return (edge.names [indices.of.edges.with.attribute.value])
    else 
      return (character(0))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyNodeAttributesFromCyGraph', 'CytoscapeConnectionClass',

  function (obj, window.id, existing.graph) {
    node.attribute.names = getNodeAttributeNames (obj)
    #node.attribute.names = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
    for (attribute.name in node.attribute.names) {
      known.node.names = xml.rpc (obj@uri, "Cytoscape.getNodes", window.id)
      nodes.with.attribute = haveNodeAttribute (obj, known.node.names, attribute.name)
      if (length (nodes.with.attribute) > 0) {
        attribute.type = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeType', attribute.name, .convert=T)
        write (sprintf ('retrieving %s "%s" attribute for %d nodes', attribute.type, attribute.name, length (nodes.with.attribute)), stderr ())
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
        else {
          write (sprintf ('RCytoscape::copyNodeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr ())
          next ()
          } 
        existing.graph = initNodeAttribute (existing.graph, attribute.name, attribute.type, default.value)
        if (length (nodes.with.attribute) == 0) next;
        if (length (nodes.with.attribute) == 1)
          attribute.values = xml.rpc (obj@uri, 'Cytoscape.getNodeAttribute', attribute.name, nodes.with.attribute)
        else
          attribute.values = xml.rpc (obj@uri, 'Cytoscape.getNodesAttributes', attribute.name, nodes.with.attribute)
        nodeData (existing.graph, nodes.with.attribute, attribute.name) = attribute.values
         } # if
      } # for

    return (existing.graph)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyEdgeAttributesFromCyGraph', 'CytoscapeConnectionClass',

  function (obj, window.id, existing.graph) {
    edge.attribute.names = getEdgeAttributeNames (obj)
    #edge.attribute.names = xml.rpc (obj@uri, 'Cytoscape.getEdgeAttributeNames', .convert=T)
    write (sprintf ('creating %d cytoscape-style edge names', length (edgeNames (existing.graph))), stderr ())
    cy2.edgenames = as.character (cy2.edge.names (existing.graph))   # < 2 seconds for > 9000 edges
  
    for (attribute.name in edge.attribute.names) {
      edges.with.attribute = haveEdgeAttribute (obj, cy2.edgenames, attribute.name)
      if (length (edges.with.attribute) > 0) {
         attribute.type = xml.rpc (obj@uri, 'Cytoscape.getEdgeAttributeType', attribute.name, .convert=T)
         write (sprintf ('retrieving %s "%s" attribute for %d edges', attribute.type, attribute.name, length (edges.with.attribute)), stderr ())
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
        else {
          write (sprintf ('RCytoscape::copyEdgeAttributesFromCyGraph, no support yet for attributes of type %s', attribute.type), stderr ())
          next ()
          } 
         existing.graph = initEdgeAttribute (existing.graph, attribute.name, attribute.type, default.value)
         eda.value = xml.rpc (obj@uri, 'Cytoscape.getEdgesAttributes', attribute.name, edges.with.attribute)
         regex = ' *[\\(|\\)] *'
         edges.tokens = strsplit (edges.with.attribute, regex)
         source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
         target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
         edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
         edgeData (existing.graph, source.nodes, target.nodes, attribute.name) = eda.value
         } # if
      } # for
  
     return (existing.graph)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getGraphFromCyWindow', 'CytoscapeConnectionClass',

  function (obj,  window.title) {
    window.id = getWindowID (obj, window.title)
    stopifnot (!is.na (window.id))
  
    all.node.names = xml.rpc (obj@uri, "Cytoscape.getNodes", window.id)
    write (sprintf ('received %d nodes from %s', length (all.node.names), window.title), stderr ())
    g = new ("graphNEL", edgemode='directed')
    write (sprintf ('adding %d nodes to local graph', length (all.node.names)), stderr ())
    g = graph::addNode (all.node.names, g)
    
    node.attribute.names = getNodeAttributeNames (obj)
    #node.attribute.names = xml.rpc (obj@uri, 'Cytoscape.getNodeAttributeNames', .convert=T)
    g = initEdgeAttribute (g, 'edgeType', 'char', 'assoc')
  

    regex = ' *[\\(|\\)] *'
    all.edge.names = xml.rpc (obj@uri, "Cytoscape.getEdges", window.id)
    write (sprintf ('received %d edges from %s', length (all.edge.names), window.title), stderr ())
    edges.tokens = strsplit (all.edge.names, regex)
  
    source.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [1]))
    target.nodes = unlist (lapply (edges.tokens, function (tokens) tokens [3]))
    edge.types =   unlist (lapply (edges.tokens, function (tokens) tokens [2]))
    write (sprintf ('adding %d edges to local graph', length (edges.tokens)), stderr ())
    g = addEdge (source.nodes, target.nodes, g)
    edgeData (g, source.nodes, target.nodes, 'edgeType') = edge.types
    g = copyNodeAttributesFromCyGraph (obj, window.id, g)
    g = copyEdgeAttributesFromCyGraph (obj, window.id, g)
  
    return (g)
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
setMethod ('addNodes', signature (obj='CytoscapeWindowClass'),

  function (obj, other.graph) {
     if (length (nodes (other.graph)) == 0) {
       write ('CytoscapeWindow.sendNodes, no nodes in other.graph.  returning', stderr ())
       return ()
       }
     new.nodes = setdiff (nodes (other.graph), nodes (obj@graph))
     invisible (xml.rpc (obj@uri, 'Cytoscape.createNodes', as.character (obj@window.id), new.nodes))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('addEdges', signature (obj='CytoscapeWindowClass'),

  function (obj, other.graph) {
    if (length (edgeNames (other.graph)) == 0) {
       write ('CytoscapeWindow.addEdges, no edges in graph.  returning', stderr ())
       return ()
       }
                 
       # extract and compare edge names
    new.edgeNames = setdiff (edgeNames (other.graph), edgeNames (obj@graph))
    printf ('---- new.edgeNames %d', length (new.edgeNames))
    print (new.edgeNames)
    new.edgeNames.withBar = gsub ('~','|', new.edgeNames)

    tokens = strsplit (new.edgeNames, '~')
    #tokens = strsplit (new.edgeNames, '~')
    #tokens = strsplit (edgeNames (other.graph), '~')
    a = sapply (tokens, function (tok) tok [1])
    b = sapply (tokens, function (tok) tok [2])
    edge.type = as.character (eda (other.graph, 'edgeType') [new.edgeNames.withBar])
    printf ('edge.type:    ')
    print (edge.type)

    if (length (edge.type) == 1 && is.na (edge.type))
      edge.type = rep ('unspecified', length (tokens))
    directed = rep (TRUE, length (tokens))
    forgive.if.node.is.missing = TRUE

     # deferring this effiency (sending only new edges) for now.
     # if ('edgeType' %in% eda.names (obj@graph) && 'edgeType' %in% eda.names (other.graph)) {
     #   existing.edge.signatures = sort (paste (names (edgeNames (obj@graph)), as.character (eda (obj@graph, 'edgeType'))))
     #   new.edge.signatures      = sort (paste (names (edgeNames (other.graph)), as.character (eda (other.graph, 'edgeType'))))
     #   new.edges = 
     #   } # if

    printf ('---- about to xml.rpc call Cytoscape.createEdges')
    print (a)
    print (b)
    print (edge.type)
    xml.rpc (obj@uri, 'Cytoscape.createEdges', as.character (obj@window.id), a, b, edge.type, directed, forgive.if.node.is.missing, .convert=F)
    }) # addEdges


#------------------------------------------------------------------------------------------------------------------------
# this method adds a new graph to an existing graph.  first the new nodes, then the new edges, then node attributes, then edge
# attributes
setMethod ('addGraphToGraph', 'CytoscapeWindowClass',

  function (obj, other.graph) {
    addNodes (obj, other.graph)  
    addEdges (obj, other.graph)
  
    node.attribute.names = noa.names (other.graph)
    for (attribute.name in node.attribute.names) {
      printf ('sending noa %s', attribute.name)
      .sendNodeAttributesForGraph (obj, other.graph, attr=attribute.name)
      }
  
    node.attribute.names = noa.names (other.graph)
    for (attribute.name in node.attribute.names) {
      printf ('sending noa %s', attribute.name)
      .sendNodeAttributesForGraph (obj, other.graph, attr=attribute.name)
      }
  
    edge.attribute.names = eda.names (other.graph)
    for (attribute.name in edge.attribute.names) {
      printf ('sending eda %s', attribute.name)
      .sendEdgeAttributesForGraph (obj, other.graph, attr=attribute.name)
      }
    }) # addGraphToGraph

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdges', 'CytoscapeWindowClass',

  function (obj) {
    if (length (edgeNames (obj@graph)) == 0) {
       write ('CytoscapeWindow.sendEdges, no edges in graph.  returning', stderr ())
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
  
    #if (length (a) == 1) {
    #  write ('doubling single edge...', stderr ())
    #  a = rep (a, 2)
    #  b = rep (b, 2)
    #  }
    
    xml.rpc (obj@uri, 'Cytoscape.createEdges', as.character (obj@window.id), a, b, edge.type, directed, forgive.if.node.is.missing, .convert=F)
    }) # sendEdges


   # for (source.node in names (edges (obj@graph))) {
   #   for (target.node in edges (obj@graph)[[source.node]]) {
   #     interaction = 'unknown'
   #     if ('edgeType' %in% names (edgeDataDefaults (obj@graph)))
   #       interaction = as.character (edgeData (obj@graph, source.node, target.node, 'edgeType'))
   #     else if ('type' %in% names (edgeDataDefaults (obj@graph)))
   #       interaction = as.character (edgeData (obj@graph, source.node, target.node, 'type'))
   #     #printf ('creating edge  %s (%s) %s', source.node, interaction, target.node)     
   #     xml.rpc (obj@uri, 'Cytoscape.createEdge', source.node, target.node, interaction, TRUE)
   #     } # for target.node
   #   } # for source.node
   # }) # sendEdges

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
      invisible (xml.rpc (obj@uri, 'Cytoscape.setNodePosition', id, node.names, as.numeric (x.coords), as.numeric (y.coords)))
    else 
      invisible (xml.rpc (obj@uri, 'Cytoscape.setNodesPositions', id, node.names, as.numeric (x.coords), as.numeric (y.coords)))

    }) # cy.setPosition

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getPosition', 'CytoscapeWindowClass',

  function (obj, node.names) {

    count = length (node.names)
    if (count == 1)
      node.names = rep (node.names, 2)   # work around R's distinction between scalar and list of strings
    raw.result = xml.rpc (obj@uri, 'Cytoscape._rGetNodesPositions', obj@window.id, node.names)
     # sample raw result (16 dec 2010): "2022:417.0,122.0" "659:156.0,0.0"   
     # now parse this list of strings into directly usable values, a named list (using node ID's) with x,y pair values

    tokens = strsplit (raw.result, ":")
    result = list ()
    for (token in tokens) {
      name = token [1]
      xy.tokens = strsplit (token [2], ",")
      x = as.integer (xy.tokens[[1]][1])
      y = as.integer (xy.tokens[[1]][2])
      result [[name]] = list (x=x, y=y)
      } # for token

    return (result)
    }) # cy.getPosition

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendNodeAttributes', 'CytoscapeWindowClass',

   function (obj, attribute.name) {

     if (length (nodes (obj@graph)) == 0)
       return ()

     #chad.debug (obj, "in RCytoscape::sendNodeAttributes")
     caller.specified.attribute.class = attr (nodeDataDefaults (obj@graph, attribute.name), 'class')

     if (is.null (caller.specified.attribute.class)) {
       msg1 = sprintf ('Error!  RCytoscape::sendNodeAttributes, attributes not initialized. You must call')
       msg2 = sprintf ('        initNodeAttribute (graph, attribute.name, attribute.type, default.value)')
       msg3 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
       msg4 = sprintf ('        example:  g <- initNodeAttribute (g, "nodeType", "char", "molecule")')
       msg5 = sprintf ('             or:  g <- initNodeAttribute (g, "pValue", "numeric", 1.0)')
       write (msg1, stderr ())
       write (msg2, stderr ())
       write (msg3, stderr ())
       write (msg4, stderr ())
       return (NA)
       }

     node.names = nodes (obj@graph)
     values = noa (obj@graph, attribute.name)
     invisible (sendNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, node.names, values))
     }) # sendNodeAttributes

#------------------------------------------------------------------------------------------------------------------------
# with this version, unlike sendNodeAttributes, the attributes need not be already stored in the graph
setMethod ('sendNodeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, node.names, values) {

     if (length (node.names) == 0)
       return ()

     #chad.debug (obj, "in RCytoscape::sendNodeAttributesDirect")

     if (length (node.names) != length (values)) {
       write (sprintf ('RCytoscape::sendNodeAttributesDirect ERROR.'), stderr ())
       write (sprintf ('attribute name %s, node.names %d, values %d', attribute.name, length (node.names), length (values)), stderr ())
       return ();
       }

        # in sending arguments to CytoscapeRPC, lists of length one become scalars, and so fail to match
        # java methods that expect lists.  to sidestep that problem, duplicate node.name and value, 
        # creating silly but effective lists of length 2

     if (length (node.names) == 1) {
       node.names = rep (node.names, 2)
       values = rep (values, 2)
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

     if (length (edgeNames (obj@graph)) == 0)
       return ()
     
     caller.specified.attribute.class = attr (edgeDataDefaults (obj@graph, attribute.name), 'class')
     #write (sprintf ('RCy::sendEdgeAttributes, eda name = %s', attribute.name), stderr ())

     if (is.null (caller.specified.attribute.class)) {
       msg1 = sprintf ('Error!  RCytoscape::sendEdgeAttributes, attributes not initialized. You must call')
       msg2 = sprintf ('        initEdgeAttribute (graph, attribute.name, attribute.type, default.value)')
       msg3 = sprintf ('        where attribute type is one of "char", "integer", or "numeric".')
       msg4 = sprintf ('        example:  g <- initEdgeAttribute (g, "edgeType", "char", "protein-protein interaction")')
       msg5 = sprintf ('             or:  g <- initEdgeAttribute (g, "confidence", "numeric", 0.0)')
       write (msg1, stderr ())
       write (msg2, stderr ())
       write (msg3, stderr ())
       write (msg4, stderr ())
       return (NA)
       }
     edge.names = as.character (cy2.edge.names (obj@graph))
     edge.names.tilde = names (cy2.edge.names (obj@graph))
     edge.names.with.bars = gsub ('~', '|', edge.names.tilde)
     values = eda (obj@graph, attribute.name) [edge.names.with.bars]
     #print (values)

     #write (edge.names, stderr ())
     #write (values, stderr ())

     invisible (sendEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, edge.names, values))
     }) # sendEdgeAttributes

#------------------------------------------------------------------------------------------------------------------------
setMethod ('sendEdgeAttributesDirect', 'CytoscapeWindowClass',

   function (obj, attribute.name, attribute.type, edge.names, values) {

     #write (sprintf ('entering sendEdgeAttributesDirect, with %d names and %d values', length (edge.names), length (values)), stderr ())

     if (length (edge.names) == 0)
       return ()

     if (length (values) == 2 * length (edge.names))
       values = values [1:length (edge.names)]

        # in sending arguments to CytoscapeRPC, lists of length one become scalars, and so fail to match
        # java methods that expect lists.  to sidestep that problem, duplicate edge.name and value, 
        # creating silly but effective lists of length 2

     if (length (edge.names) == 1) {
       edge.names = rep (edge.names, 2)
       values = rep (values, 2)
       }

     #write (sprintf ('edge.names: %s', list.to.string (edge.names)), stderr ())
     #write (sprintf ('    values: %s', list.to.string (values)), stderr ())

     caller.specified.attribute.class = tolower (attribute.type)

     if (is.null (caller.specified.attribute.class) || length (caller.specified.attribute.class) == 0)   # NULL, or non-null but empty
       caller.specified.attribute.class = 'string'

     result = ''

     if (length (edge.names) != length (values)) {
       write (sprintf ('RCytoscape::sendEdgeAttributesDirect ERROR....'), stderr ())
       write (sprintf ('attribute name %s, edge.names %d, values %d', attribute.name, length (edge.names), length (values)), stderr ())
       return ();
       }

     if (caller.specified.attribute.class %in% c ('floating', 'numeric', 'double')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addDoubleEdgeAttributes', attribute.name, edge.names, as.numeric (values), .convert=TRUE)
       #write (sprintf ('result of addDoubleEdgeAttributes: %s', result), stderr ())
       #write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('integer', 'int')) {
       result = xml.rpc (obj@uri, 'Cytoscape.addIntegerEdgeAttributes', attribute.name, edge.names, as.integer (values), .convert=TRUE)
       #write (sprintf ('result of addIntegerEdgeAttributes: %s', result), stderr ())
       #write (result, stderr ())
       }
     else if (caller.specified.attribute.class %in% c ('string', 'char', 'character')) {
       if (length (edge.names) == 1)
         result = xml.rpc (obj@uri, 'Cytoscape.addStringEdgeAttribute', attribute.name, edge.names, as.character (values), .convert=TRUE)
       else
         result = xml.rpc (obj@uri, 'Cytoscape.addStringEdgeAttributes', attribute.name, edge.names, as.character (values), .convert=TRUE)
       #write (sprintf ('result of addStringEdgeAttribute/s: %s', result), stderr ())
       #write (result, stderr ())
       }

     invisible (result)
     }) # sendEdgeAttributesDirect

#------------------------------------------------------------------------------------------------------------------------
setMethod ('displayGraph', 'CytoscapeWindowClass',

   function (obj) {
     write ('entering RCytoscape::displayGraph', stderr ())
     #chad.debug (obj, "starting displayGraph")     
     if (length (nodes (obj@graph)) == 0) {
       write ('RCytoscape::displayGraph, empty graph, returning', stderr ())
       return ()
       }

     write (sprintf ('adding %d nodes...', length (nodes (obj@graph))),  stderr ())
     sendNodes (obj)
     write (sprintf ('adding %d edges...', length (edgeNames (obj@graph))), stderr ())
     sendEdges (obj)
     #chad.debug (obj, "just before adding node attributes")
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
setMethod ('hidePanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.hidePanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideAllPanels', 'CytoscapeConnectionClass',

  function (obj) {
    invisible (sapply (tolower (LETTERS), function (letter) hidePanel (obj, letter)))
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('dockPanel', 'CytoscapeConnectionClass',

   function (obj, panelName) {
     invisible (xml.rpc (obj@uri, 'Cytoscape.dockPanel', panelName))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('floatPanel', 'CytoscapeConnectionClass',

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
setMethod ('setEdgeLabelRule', 'CytoscapeWindowClass',

  function (obj, edge.attribute.name) {
    id = as.character (obj@window.id)
    default.value = ''
    result = xml.rpc (obj@uri, 'Cytoscape.edgePassthroughMapper', edge.attribute.name, 'Edge Label', default.value)
    invisible (result)
    })  # setEdgeLabelRule

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
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         colors = rep (colors, 2)
         } 
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
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         colors = rep (colors, 2)
         } 
       result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', as.character (obj@window.id), default.style, 
                         node.attribute.name, 'Node Border Color', default.color, control.points, colors)
       invisible (result)
       } # else: !interpolate
     }) # setNodeBorderColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setNodeBorderWidthRule', 'CytoscapeWindowClass',

   function (obj, node.attribute.name, attribute.values, line.widths, default.width=1) {

     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       }
     id = as.character (obj@window.id)
     visual.property.type.name = 'Node Line Width'  # see class cytoscape.visual.VisualPropertyType

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       } 
     result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', id, 'default', node.attribute.name, 
                       'Node Line Width', as.character (default.width), attribute.values, as.character (line.widths))
     invisible (result)
     })

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
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       node.shapes = rep (node.shapes, 2)
       }
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
       if (length (control.points) == 1) {   # code around the requirement that one-element lists are turned into scalars
         control.points = rep (control.points, 2)
         node.sizes = rep (node.sizes, 2)
         } 

       result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', as.character (obj@window.id), default.style, 
                         node.attribute.name, 'Node Size', as.character (default.size), 
                         as.character (control.points), as.character (node.sizes))
       invisible (result)
       } # else: !interpolate
     }) # setNodeSizeRule


#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeColorRule', 'CytoscapeWindowClass',

 function (obj, attribute.name, attribute.values, colors, default.color='#000000') {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       colors = rep (colors, 2)
       }
     setDefaultEdgeColor (obj, default.color)
     id = as.character (obj@window.id)
     default.color = '#000000'
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeColorRule", id, attribute.name, default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setEdgeColorRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineStyleRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, attribute.values, line.styles, default.style='SOLID') {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       line.styles = rep (line.styles, 2)
       }
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeLineStyleRule", id, edge.attribute.name, default.style, 
                       attribute.values, line.styles, .convert=TRUE)
     invisible (result)
     }) # set.edge.line.style.rule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeLineWidthRule', 'CytoscapeWindowClass',

   function (obj, edge.attribute.name, attribute.values, line.widths, default.width=1) {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       }
     id = as.character (obj@window.id)
     visual.property.type.name = 'Edge Line Width'  # see class cytoscape.visual.VisualPropertyType

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       line.widths = rep (line.widths, 2)
       } 
     result = xml.rpc (obj@uri, 'Cytoscape.discreteMapper', id, 'default', edge.attribute.name, 
                       'Edge Line Width', as.character (default.width), attribute.values, as.character (line.widths))
     invisible (result)
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
     # write (sprintf ('before -- attribute.values: %d   arrows: %d', length (attribute.values), length (arrows)), stderr ())
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       arrows = rep (arrows, 2)
       }
     #write (sprintf ('after -- attribute.values: %d   arrows: %d', length (attribute.values), length (arrows)), stderr ())
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeTargetArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, arrows, default='Arrow') {
     if (length (attribute.values) == 1) {  # hack: list of length 1 treated as scalar, failing method match -- double into a list
       attribute.values = rep (attribute.values, 2)
       arrows = rep (arrows, 2)
       }
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, "Cytoscape.setEdgeSourceArrowRule", id, edge.attribute.name, default, attribute.values, arrows, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeTargetArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
     id = as.character (obj@window.id)
     style.name = 'default'

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       colors = rep (colors, 2)
       } 

     result = xml.rpc (obj@uri, "Cytoscape.discreteMapper", id, style.name, edge.attribute.name,
                      'Edge Target Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setEdgeSourceArrowColorRule', 'CytoscapeWindowClass', 

   function (obj, edge.attribute.name, attribute.values, colors, default.color='#000000') {
     id = as.character (obj@window.id)
     style.name = 'default'

     if (length (attribute.values) == 1) {   # code around the requirement that one-element lists are turned into scalars
       attribute.values = rep (attribute.values, 2)
       colors = rep (colors, 2)
       } 

     result = xml.rpc (obj@uri, "Cytoscape.discreteMapper", id, style.name, edge.attribute.name,
                      'Edge Source Arrow Color', default.color, attribute.values, colors, .convert=TRUE)
     invisible (result)
     }) # setTargetArrowRule

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeCount', 'CytoscapeWindowClass',
   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countNodes", id)
     return (count)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeCount', 'CytoscapeWindowClass',
   function (obj) {
     id = as.character (obj@window.id)
     count = xml.rpc (obj@uri, "Cytoscape.countEdges", id)
     return (count)
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeAttribute', 'CytoscapeConnectionClass',

   function (obj, node.name, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.getNodeAttribute", node.name, attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeAttribute', 'CytoscapeConnectionClass',

   function (obj, edge.name, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.getEdgeAttribute", edge.name, attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getNodeAttributeNames', 'CytoscapeConnectionClass',

   function (obj) {
     return (xml.rpc (obj@uri, "Cytoscape.getNodeAttributeNames"))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('getEdgeAttributeNames', 'CytoscapeConnectionClass',

   function (obj) {
     return (xml.rpc (obj@uri, "Cytoscape.getEdgeAttributeNames"))
     })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteNodeAttribute', 'CytoscapeConnectionClass',

   function (obj, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.deleteNodeAttribute", attribute.name))
     })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('deleteEdgeAttribute', 'CytoscapeConnectionClass',

   function (obj, attribute.name) {
     return (xml.rpc (obj@uri, "Cytoscape.deleteEdgeAttribute", attribute.name))
     })

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
setMethod ('clearSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.clearSelection', id, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # clearSelection
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('selectNodes', 'CytoscapeWindowClass',

   function (obj, node.names) {
     id = as.character (obj@window.id)
     result = xml.rpc (obj@uri, 'Cytoscape.selectNodes',id, node.names, .convert=TRUE)
     redraw (obj)
     invisible (result)
     }) # selectNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedNodeCount', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.countSelectedNodes', id, .convert=TRUE))
     }) # countSelectedNodes
   
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
setMethod ('hideSelectedNodes', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedNodes', id, .convert=TRUE))
     }) # hideSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('invertNodeSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.invertNodeSelection', id, .convert=TRUE))
     }) # invertNodeSelection
 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('removeSelectedNodes', 'CytoscapeWindowClass',

   function (obj, remove.from.root.graph.also=TRUE) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.removeSelectedNodes', id, remove.from.root.graph.also, .convert=TRUE))
     }) # removeSelectedNodes
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('invertEdgeSelection', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.invertEdgeSelection', id, .convert=TRUE))
     }) # invertEdgeSelection
 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('removeSelectedEdges', 'CytoscapeWindowClass',

   function (obj, remove.from.root.graph.also=TRUE) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.removeSelectedEdges', id, remove.from.root.graph.also, .convert=TRUE))
     }) # removeSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdgeCount', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     return (xml.rpc (obj@uri, 'Cytoscape.countSelectedEdges', id, .convert=TRUE))
     }) # countSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     if (getSelectedEdgeCount (obj) == 0)
       return (NA)
     else {
       result = xml.rpc (obj@uri, 'Cytoscape.getSelectedEdges', id, .convert=TRUE)
       return (result)
       }
     }) # getSelectedEdges
   
#------------------------------------------------------------------------------------------------------------------------
setMethod ('hideSelectedEdges', 'CytoscapeWindowClass',

   function (obj) {
     id = as.character (obj@window.id)
     invisible (xml.rpc (obj@uri, 'Cytoscape.hideSelectedEdges', id, .convert=TRUE))
     }) # hideSelectedEdges
   
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
setMethod ('cy.sfn', 'CytoscapeWindowClass',

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
     }) # cy.sfn

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
  if (!node.attribute.name %in% noa.names (graph))
    return (NA)

  return (unlist (nodeData (graph, attr=node.attribute.name)))

} # noa
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
  #printf ('running new version of cy2.edge.names')
  if (length (edges (graph)) == 0)
    return (NA)

  edgeType.attribute.present = TRUE
  edge.type = 'unspecified'
  if ('edgeType' %in% names (edgeDataDefaults (graph))) {
     edge.type = as.character (eda (graph, 'edgeType'))
     }

  tokens = strsplit (edgeNames (graph), '~')
  a = sapply (tokens, function (tok) tok [1])
  b = sapply (tokens, function (tok) tok [2])
  edge.type = paste (' (', edge.type, ') ', sep='')
  edge.names = paste (a, edge.type, b, sep='')

  names (edge.names) = edgeNames (graph)
  return (edge.names)

} # cy2.edge.names
#------------------------------------------------------------------------------------------------------------------------
old.cy2.edge.names = function (graph)
{
  edgeType.attribute.present = TRUE
  if (!'edgeType' %in% names (edgeDataDefaults (graph))) {
     edge.type = 'unspecified'
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

} # old.cy2.edge.names
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
  attr (edgeDataDefaults (g, attr="weight"), "class") = "DOUBLE"
  edgeDataDefaults (g, 'pmid') = '9988778899'
  attr (edgeDataDefaults (g, attr="pmid"), "class") = "STRING"
  return (g)

} # makeRandomGraph
#------------------------------------------------------------------------------------------------------------------------
# the bioconductor graph class stores undirected graph edge attributes redundantly.  bioc's nishant says (email, 2 sep 2010):
#
# The people who started the graph package decided to return duplicate edge attributes / weights for the undirected
# case. ie if you have an edge a-b and the graph is undirected, methods such as edgeWeights, edgeData etc will end up
# returning duplicate values for the attribute for a-b and b-a.  That was a design decision taken by the creators of the
# package and I do not think it will be possible to change that now.  I guess the solution might be to create your own
# edgeWeights and edgeData methods in your package that retrieve only the non-duplicated attributes for the undirected
# case.
#
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
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been updated with new ones from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new nodes.
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new nodes, use the standard method 'sendNodeAttributesDirect' to send them to cytoscape
# 
.sendNodeAttributesForGraph = function (obj, other.graph, attribute.name)
{
  caller.specified.attribute.class = attr (nodeDataDefaults (other.graph, attribute.name), 'class')

  if (is.null (caller.specified.attribute.class)) {
    msg1 = sprintf ('Error!  RCytoscape:::.sendNodeAttributesForGraph. You must initialize the "%s" node attribute.', attribute.name)
    msg2 = sprintf ('        example:  my.graph = initNodeAttribute (my.graph, attr="moleculeType", "char", "unspecified")')
    write (msg1, stderr ())
    write (msg2, stderr ())
    return (NA)
    }

     # only add attributes for new nodes, unique to the new graph 'other.graph'
   new.node.names = setdiff (nodes (other.graph), nodes (obj@graph))
   values = noa (other.graph, attribute.name) [new.node.names]
   invisible (sendNodeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, new.node.names, values))

} # .sendNodeAttributesForGraph 
#------------------------------------------------------------------------------------------------------------------------
# used when adding a new graph to an existing graph.  we assume (but do not yet here test) that before this method
# is called, the Cytoscape graph has already been extended with all the new nodes and edges from 'other.graph'
# there may be some overlap between the two graphs; care is taken to only send attributes for new edges
# pre-existing attributes in the old graph are therefore not affected.
# the strategy:  identify the new edges, use the standard method 'sendEdgeAttributesDirect' to send them to cytoscape
# oddities: edge naming is a tricky business.  cytoscape lablels edges like this:
#    <sourceNode> (interactionType) <targetNode>
# RCytoscape provide a utility function for retrieving them from an R graph object,   cy2.edge.names (g)
# which uses the edgeNames (g) method to get the R names
# edgeNames (g2)  # [1] "A~E" "A~B" "D~E"
# thus, R has a little inconsistency:  sometimes using the tilda, sometimes the vertical bar
#                 A~E                 A~B                 D~E 
#     "A (inferred) E" "A (unspecified) B"  "D (literature) E" 
# names (edgeData (g2, attr='edgeType'))
#    [1] "A|E" "A|B" "D|E"
# for historical reasons, and maybe laziness, these two conventions are supported here, at the cost of calling gsub on the edge
# names, so that A~E becomes A|E, setting the stage for calling 
#   values = eda (g, attribute.name) [new.edge.names.with.bar.delimitor]
# below, and thereby ensuring that only the attributes of new edges are sent to Cytoscape

.sendEdgeAttributesForGraph = function (obj, other.graph, attribute.name)
{
  caller.specified.attribute.class = attr (edgeDataDefaults (other.graph, attribute.name), 'class')

  if (is.null (caller.specified.attribute.class)) {
    msg1 = sprintf ('Error!  RCytoscape:::.sendEdgeAttributesForGraph. You must initialize the "%s" edge attribute.', attribute.name)
    msg2 = sprintf ('        example:  my.graph = initEdgeAttribute (my.graph, attr="edgeType", "char", "unspecified")')
    write (msg1, stderr ())
    write (msg2, stderr ())
    return (NA)
    }
     # send only attributes for edges which are unique to other.graph; we assume that any existing edges already have their attributes

   new.edge.names.compact = setdiff (names (cy2.edge.names (other.graph)), names (cy2.edge.names (obj@graph)))
   if (length (new.edge.names.compact) == 0) 
     return

   new.edge.names.cy2.style = setdiff (as.character (cy2.edge.names (other.graph)), as.character (cy2.edge.names (obj@graph)))

   new.edge.names.with.bar.delimitor = gsub ('~', '|', new.edge.names.compact)
   values = eda (other.graph, attribute.name) [new.edge.names.with.bar.delimitor]
   #write (sprintf ('sending edge attributes direct for attr %s', attribute.name), stderr ())
   #write (new.edge.names.compact, stderr ())
   #write (new.edge.names.with.bar.delimitor, stderr ())
   #write ('---- new.edge.names.cy2.style', stderr ())
   #write (new.edge.names.cy2.style, stderr ())
   #write (values, stderr ())

   invisible (sendEdgeAttributesDirect (obj, attribute.name, caller.specified.attribute.class, new.edge.names.cy2.style, values))

} # .sendEdgeAttributesForGraph 
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getVisualStyleNames', 'CytoscapeConnectionClass',

  function (obj) {
    result = xml.rpc (obj@uri, 'Cytoscape.getVisualStyleNames')
    return (result)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('copyVisualStyle', 'CytoscapeConnectionClass',

  function (obj, from.style, to.style) {
    current.names = getVisualStyleNames (obj)
    if (! from.style %in% current.names)
      stop (sprintf ('Cannot copy from a non-existent visual style (%s)', from.style))
    xml.rpc (obj@uri, 'Cytoscape.copyVisualStyle', from.style, to.style)
    })

#------------------------------------------------------------------------------------------------------------------------
setMethod ('setVisualStyle', 'CytoscapeConnectionClass',

  function (obj, new.style.name) {
    current.names = getVisualStyleNames (obj)
    if (! new.style.name %in% current.names)
      stop (sprintf ('Cannot call setVisualStyle on a non-existent visual style (%s)', new.style.name))
    xml.rpc (obj@uri, 'Cytoscape.setVisualStyle', new.style.name)
    })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultBackgroundColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultBackgroundColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultBackgroundColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultBackgroundColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultNodeSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultNodeSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultNodeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultNodeReverseSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultNodeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color,vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultNodeReverseSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultEdgeSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultEdgeSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('getDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, vizmap.style.name='default') {
      return (xml.rpc (obj@uri, 'Cytoscape.getDefaultEdgeReverseSelectionColor', vizmap.style.name))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('setDefaultEdgeReverseSelectionColor',  'CytoscapeConnectionClass',

   function (obj, new.color, vizmap.style.name='default') {
      invisible (xml.rpc (obj@uri, 'Cytoscape.setDefaultEdgeReverseSelectionColor', vizmap.style.name, new.color))
      })
#------------------------------------------------------------------------------------------------------------------------
setMethod ('saveImage', 'CytoscapeWindowClass',

   function (obj, file.name, image.type, scale) {
     id = as.character (obj@window.id)
      return (xml.rpc (obj@uri, 'Cytoscape.exportView', id, file.name, image.type, scale))
      })
#------------------------------------------------------------------------------------------------------------------------
chad.debug = function (obj, msg)
{
  write ('special debug version for chad burrus', stderr ())
  write (msg, stderr ())
  write ('--- node attribute names', stderr ())

  node.attribute.names = noa.names (obj@graph)
  write (node.attribute.names, stderr ())

  for (noa.name in node.attribute.names) {
     if (noa.name == 'label') next
     write (sprintf (' ** %s', noa.name), stderr ())
     write (unlist (nodeData (obj@graph, attr=noa.name)), stderr ())
     }

} # chad.debug
#------------------------------------------------------------------------------------------------------------------------
