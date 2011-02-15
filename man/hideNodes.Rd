\name{hideNodes}
\alias{hideNodes}
\alias{hideNodes,CytoscapeWindowClass-method}
\title{hideNodes}
\description{
Hide (but do not delete) the currently  nodes.   'Unhide' is
supposed to return them to view, but this is broken in Cytoscape 2.7.
}
\usage{
hideNodes(obj, node.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{character} list object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  hideSelectedNodes
  unhideNodes
}

\examples{
  cw <- new.CytoscapeWindow ('hideNodes.test', graph=makeSimpleGraph())
  redraw (cw)
  layout (cw, 'jgraph-spring')
  hideNodes (cw, c ('A', 'B'))

}


\keyword{graph}
