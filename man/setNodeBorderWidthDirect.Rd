\name{setNodeBorderWidthDirect}
\alias{setNodeBorderWidthDirect}
\alias{setNodeBorderWidthDirect,CytoscapeWindowClass-method}
\title{setNodeBorderWidthDirect}
\description{
In the specified CytoscapeWindow, set the width of the border of the specified node.
}
\usage{
setNodeBorderWidthDirect(obj, node.names, new.sizes)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.sizes}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeSizeDirect
 }

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('setNodeBorderWidthDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setNodeBorderWidthDirect (cw, 'A', 10)
  redraw (cw)
}}



\keyword{graph}
