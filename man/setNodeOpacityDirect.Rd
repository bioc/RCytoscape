\name{setNodeOpacityDirect}
\alias{setNodeOpacityDirect}
\alias{setNodeOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of all aspectes of the
specified node:  fill color, border, label.
}
\usage{
setNodeOpacityDirect(obj, node.names, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.value}{a \code{numeric} object, ranging from 0 to 255.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeFillOpacityDirect
  setNodeLabelOpacityDirect
  setNodeBorderOpacityDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeOpacityDirect (cw, 'A', 220)
  redraw (cw)
}


\keyword{graph}
