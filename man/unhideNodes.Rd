\name{unhideNodes}
\alias{unhideNodes}
\alias{unhideNodes,CytoscapeWindowClass-method}
\title{unhideNodes}
\description{
Unhide (but do not delete) the currently  nodes.   'Ununhide' is
supposed to return them to view, but this is broken in Cytoscape 2.7.
}
\usage{
unhideNodes(obj, node.names)
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
  hideNodes
}

\examples{
  cw <- new.CytoscapeWindow ('unhideNodes.test', graph=makeSimpleGraph())
  layout (cw, 'jgraph-spring')
  redraw (cw)
  hideNodes (cw, c ('A', 'B'))
  system ('sleep 1')
  unhideNodes (cw, c ('A', 'B'))
}


\keyword{graph}
