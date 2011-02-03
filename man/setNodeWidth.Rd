\name{setNodeWidth}
\alias{setNodeWidth}
\alias{setNodeWidth,CytoscapeWindowClass-method}
\title{setNodeWidth}
\description{
In the specified CytoscapeWindow, set the width of the specified node.
Not that the node dimensions (width and width) must be unlocked for
this to work.  If they ARE locked, then node and width change together,
as specified by a node size rule, or the setNodeSize method
}
\usage{
setNodeWidth(obj, node.name, new.width)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.name}{a \code{String} object.}
  \item{new.width}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeWidth
  lockNodeDimensions
  setNodeSize
  setNodeWidth
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeWidth.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  lockNodeDimensions (cw, 'default', FALSE)
  setNodeWidth (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
