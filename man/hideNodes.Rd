\name{hideNodes}
\alias{hideNodes}
\alias{hideNodes,CytoscapeWindowClass-method}
\title{hideNodes}
\description{
Hide (but do not delete) the currently  nodes.  Highly recommended: 
save the current layout before hiding, since 'unhideAll' will, in
addition to restoring hidden nodes to view, will place then in
unpredictable locations on the screen.
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
  unhideAll
  saveLayout
  restoreLayout
}

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('hideNodes.test', graph=makeSimpleGraph())
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  # saveLayout (cw, 'layout.tmp.RData')
  hideNodes (cw, c ('A', 'B'))
  unhideAll (cw)
  # restoreLayout (cw, 'layout.tmp.RData')

}}



\keyword{graph}
