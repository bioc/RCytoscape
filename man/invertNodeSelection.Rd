\name{invertNodeSelection}
\alias{invertNodeSelection}
\alias{invertNodeSelection,CytoscapeWindowClass-method}
\title{invertNodeSelection}
\description{
Select the specified nodes.
}
\usage{
invertNodeSelection(obj)
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
  cw <- new.CytoscapeWindow ('invertNodeSelection demo', graph=makeSimpleGraph())
     # all nodes should be selected, since none were before
  selectNodes (cw, 'A')
  invertNodeSelection (cw)
  redraw (cw)
     # a richer test will be to programmatically select nodes, but that
     #  does not work yet (pshannon, 13 jan 2011)
   
}

\keyword{graph}
