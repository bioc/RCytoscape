\name{setEdgeOpacityDirect}
\alias{setEdgeOpacityDirect}
\alias{setEdgeOpacityDirect,CytoscapeWindowClass-method}
\title{setEdgeOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the specified edge
or edges.  Low numbers, near zero, are transparent.
High numbers, near 255, are maximally opaque:  they are fully visible.
}
\usage{
setEdgeOpacityDirect(obj, edge.names, new.values)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{edge.names}{one or more \code{String} objects, cy2-style edge names.}
  \item{new.values}{a \code{numeric} object, ranging from 0 to 255.}
}

\value{
None.
}
\author{Paul Shannon}


\seealso{
  setNodeOpacityDirect
 }

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('setEdgeOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  edge.names = as.character (cy2.edge.names (cw@graph)) [1:2]
  for (i in 1:10) {
    setEdgeOpacityDirect (cw, edge.names, 255 - (i * 25))
    redraw (cw)
    }

  for (i in 1:10) {
    setEdgeOpacityDirect (cw, edge.names, i * 25)
    redraw (cw)
    }
}
}

\keyword{graph}
