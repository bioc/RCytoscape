\name{firstNeighbors}
\alias{firstNeighbors}
\alias{firstNeighbors,CytoscapeWindowClass-method}
\title{firstNeighbors}
\description{
Returns the first neighbors of the specified node, using the names with
which the original R graph was built.  Nodes are often displayed in
Cytoscape using a label which is different than the node's true name; be
careful of this distinction.
}
\usage{
firstNeighbors(obj, nodeName)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{nodeName}{a character string }
}

\value{
A list of the names of nodes which share an edge with the supplied nodeName.
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('firstNeighbors.test', graph=makeSimpleGraph())
  firstNeighbors (cw, 'A')
    # $right
    #  [1] "B"
    # $left
    #  [1] "C"
}

\keyword{graph}
