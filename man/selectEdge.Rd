\name{selectEdge}
\alias{selectEdge}
\alias{selectEdge,CytoscapeWindowClass-method}
\title{selectEdge}
\description{
Select the specified edge.  
}
\usage{
selectEdge(obj, edge.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.name}{a \code{character string}, the names of the singleedge to select. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  getSelectedEdgeCount
  selectEdges
  getSelectedEdges
  hideSelectedEdges
}

\examples{
  cw <- new.CytoscapeWindow ('selectEdge.test', graph=makeSimpleGraph())
  displayGraph (cw); layout (cw); redraw (cw)
  clearSelection (cw)
  selectEdge (cw, c ("A (phosphorylates) B"))
  getSelectedEdges (cw)
}

\keyword{graph}
