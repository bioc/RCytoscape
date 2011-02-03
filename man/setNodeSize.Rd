\name{setNodeSize}
\alias{setNodeSize}
\alias{setNodeSize,CytoscapeWindowClass-method}
\title{setNodeSize}
\description{
In the specified CytoscapeWindow, set the size of the specified node.
Not that the node dimensions (size and size) must be locked (the default
state) for this to work.  Node height and width change together.
}
\usage{
setNodeSize(obj, node.name, new.size)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.name}{a \code{String} object.}
  \item{new.size}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  lockNodeDimensions
  setNodeWidth
  setNodeHeight
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeSize.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeSize (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
