\name{getSelectedNodes}
\alias{getSelectedNodes}
\alias{getSelectedNodes,CytoscapeWindowClass-method}
\title{getSelectedNodes}
\description{
Retrieve the identifiers of all the nodes selected in the current graph.
}
\usage{
getSelectedNodes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings.
}
\author{Paul Shannon}

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('getSelectedNodes.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
    # in Cytoscape, interactively select two nodes, or
  selectNodes (cw, c ('A','B'))
  getSelectedNodes (cw)
  # [1] "A" "B"
}}


\keyword{graph}
