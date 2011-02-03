\name{setNodeHeight}
\alias{setNodeHeight}
\alias{setNodeHeight,CytoscapeWindowClass-method}
\title{setNodeHeight}
\description{
In the specified CytoscapeWindow, set the height of the specified node.
Not that the node dimensions (height and width) must be unlocked for
this to work.  If they ARE locked, then node and height change together,
as specified by a node size rule, or the setNodeSize method
}
\usage{
setNodeHeight(obj, node.name, new.height)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.name}{a \code{String} object.}
  \item{new.height}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeWidth
  lockNodeDimensions
  setNodeSize
  setNodeHeight
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeHeight.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  lockNodeDimensions (cw, 'default', FALSE)
  setNodeHeight (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
