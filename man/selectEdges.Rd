\name{selectEdges}
\alias{selectEdges}
\alias{selectEdges,CytoscapeWindowClass-method}
\title{selectEdges}
\description{
Select the specified edges.
}
\usage{
selectEdges(obj, edge.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{a \code{list} of strings, the names of edges to select. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  selectEdge
  getSelectedEdgeCount
  getSelectedEdges
  hideSelectedEdges
}

\examples{
  cw <- new.CytoscapeWindow ('selectEdges.test', graph=makeSimpleGraph())
  displayGraph (cw); layout (cw); redraw (cw)
  clearSelection (cw)
  selectEdges (cw, c ("A (phosphorylates) B", "B (synthetic lethal) C"))
  getSelectedEdges (cw)
}

\keyword{graph}
