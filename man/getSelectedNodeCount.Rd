\name{getSelectedNodeCount}
\alias{getSelectedNodeCount}
\alias{getSelectedNodeCount,CytoscapeWindowClass-method}
\title{getSelectedNodeCount}
\description{
Returns the number of node currently selected.
}
\usage{
getSelectedNodeCount(obj)
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
  cw <- new.CytoscapeWindow ('getSelectedNodeCount.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
    # in Cytoscape, interactively select two nodes, or
  selectNodes (cw, c ('A','B'))
  getSelectedNodeCount (cw)
  # [1] 2
}}


\keyword{graph}
