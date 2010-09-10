\name{sfn}
\alias{sfn}
\alias{sfn,CytoscapeWindowClass-method}
\title{sfn}
\description{
In the Cytoscape window, select all first neighbors of the curently
selected nodes.
}
\usage{
sfn(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getSelectedNodeCount
  selectNodes
  getSelectedNodes
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  selectNodes (cw, 'A')
  sfn (cw)
  getSelectedNodes (cw)  # should be c ('A', 'B', 'C')
}


\keyword{graph}
