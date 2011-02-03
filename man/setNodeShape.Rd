\name{setNodeShape}
\alias{setNodeShape}
\alias{setNodeShape,CytoscapeWindowClass-method}
\title{setNodeShape}
\description{
In the specified CytoscapeWindow, set the shape of the specified node.
}
\usage{
setNodeShape(obj, node.name, new.shape)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.name}{a \code{String} object.}
  \item{new.shape}{a \code{String} object, one of the allowed values
    returned by getNodeShapes.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  getNodeShapes
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeShape.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeShape (cw, 'A', 'triangle')
  redraw (cw)
}


\keyword{graph}
