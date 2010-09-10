\name{hideSelectedNodes}
\alias{hideSelectedNodes}
\alias{hideSelectedNodes,CytoscapeWindowClass-method}
\title{hideSelectedNodes}
\description{
Hide (but do not delete) the currently selected nodes.   'Unhide' is
supposed to return them to view, but this is broken in Cytoscape 2.7.
}
\usage{
hideSelectedNodes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  unhideAll
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  selectNodes (cw, c ('A', 'B'))
  hideSelectedNodes (cw)
  unhideAll (cw)
   # alas, Cytoscape requires that you render these nodes, and redo the
   # layout, no that they are visible again
  redraw (cw)
  layout (cw, 'jgraph-spring')
}


\keyword{graph}
