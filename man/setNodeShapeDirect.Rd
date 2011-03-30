\name{setNodeShapeDirect}
\alias{setNodeShapeDirect}
\alias{setNodeShapeDirect,CytoscapeWindowClass-method}
\title{setNodeShapeDirect}
\description{
In the specified CytoscapeWindow, set the shape of the specified node.
}
\usage{
setNodeShapeDirect(obj, node.names, new.shape)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.shape}{a \code{String} object, one of the allowed values
    returned by getNodeShapeDirects.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getNodeShapeDirects
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeShapeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeShapeDirect (cw, 'A', 'triangle')
  redraw (cw)
}


\keyword{graph}
