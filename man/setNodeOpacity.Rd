\name{setNodeOpacity}
\alias{setNodeOpacity}
\alias{setNodeOpacity,CytoscapeWindowClass-method}
\title{setNodeOpacity}
\description{
In the specified CytoscapeWindow, set the opacity of the specified node.
}
\usage{
setNodeOpacity(obj, node.name, new.value)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.name}{a \code{String} object.}
  \item{new.value}{a \code{numeric} object, ranging from 0 to 255.}
}

\value{
None.
}
\author{Paul Shannon}


\examples{
  cw <- new.CytoscapeWindow ('setNodeOpacity.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeOpacity (cw, 'A', 220)
  redraw (cw)
}


\keyword{graph}
