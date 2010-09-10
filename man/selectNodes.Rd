\name{selectNodes}
\alias{selectNodes}
\alias{selectNodes,CytoscapeWindowClass-method}
\title{selectNodes}
\description{
Select the specified nodes.
}
\usage{
selectNodes(obj, node.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{list} of strings, the names of nodes to select. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  getSelectedNodeCount
  getSelectedNodes
  hideSelectedNodes
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  clearSelection (cw)
  selectNodes (cw, c ('A', 'B'))
  getSelectedNodes (cw)
    # [1] "A" "B"
}

\keyword{graph}
