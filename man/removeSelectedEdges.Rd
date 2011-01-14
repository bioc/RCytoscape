\name{removeSelectedEdges}
\alias{removeSelectedEdges}
\alias{removeSelectedEdges,CytoscapeWindowClass-method}
\title{removeSelectedEdges}
\description{
Select the specified nodes.
}
\usage{
removeSelectedEdges(obj, remove.from.root.graph.also=TRUE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{remove.from.root.graph.also}{a \code{boolean} object, default TRUE }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  invertNodeSelection
}

\examples{
  cw <- new.CytoscapeWindow ('removeSelectedEdges demo', graph=makeSimpleGraph())
     # all edges should be selected, since none were before.
     # then all will be removed
  invertEdgeSelection (cw)
  removeSelectedEdges (cw)
  redraw (cw)
     # a richer test will be to programmatically select edges, but that
     #  does not work yet (pshannon, 13 jan 2011)
   
}

\keyword{graph}
