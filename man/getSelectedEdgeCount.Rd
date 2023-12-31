\name{getSelectedEdgeCount}
\alias{getSelectedEdgeCount}
\alias{getSelectedEdgeCount,CytoscapeWindowClass-method}
\title{getSelectedEdgeCount}
\description{
Returns the number of edge currently selected.
}
\usage{
getSelectedEdgeCount(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
An integer.
}
\author{Paul Shannon}

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('getSelectedEdgeCount.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
  clearSelection (cw)
  getSelectedEdgeCount (cw) # should be 0
    # in Cytoscape, interactively select an edge, or programmatically (doesn't work yet)
    # selectEdges (cwe, "A (phosphorylates) B")
  getSelectedEdgeCount (cw)
  # should be 1
}}


\keyword{graph}
