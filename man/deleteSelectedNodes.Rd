\name{deleteSelectedNodes}
\alias{deleteSelectedNodes}
\alias{deleteSelectedNodes,CytoscapeWindowClass-method}
\title{deleteSelectedNodes}
\description{In Cytoscape, delete all the selected nodes.  Edges originating
  or terminating in these nodes will be deleted also.  The nodes will
  still exist in the corresponding R graph until you explicitly delete them there
  as well.
}
\usage{
deleteSelectedNodes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  selectNodes
  deleteSelectedEdges
 }

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('deleteSelectedNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring') 
  print (nodes (cw@graph))
  selectNodes (cw, "B")
  deleteSelectedNodes (cw)
}}



\keyword{graph}
