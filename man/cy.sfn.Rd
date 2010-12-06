\name{cy.sfn}
\alias{cy.sfn}
\alias{cy.sfn,CytoscapeWindowClass-method}
\title{cy.sfn}
\description{
In the Cytoscape window, select all first neighbors of the curently
selected nodes.
}
\usage{
cy.sfn(obj)
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
  cw <- new.CytoscapeWindow ('cy.sfn.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  selectNodes (cw, 'A')
  cy.sfn (cw)
  getSelectedNodes (cw)  # should be c ('A', 'B', 'C')
}


\keyword{graph}
