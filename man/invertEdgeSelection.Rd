\name{invertEdgeSelection}
\alias{invertEdgeSelection}
\alias{invertEdgeSelection,CytoscapeWindowClass-method}
\title{invertEdgeSelection}
\description{
Select the specified nodes.
}
\usage{
invertEdgeSelection(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
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
  cw <- new.CytoscapeWindow ('invertEdgeSelection demo', graph=makeSimpleGraph())
     # all edges should be selected, since none were before
  invertEdgeSelection (cw)
  redraw (cw)
     # a richer test will be to programmatically select edges, but that
     #  does not work yet (pshannon, 13 jan 2011)
   
}

\keyword{graph}
