\name{setNodeHeightDirect}
\alias{setNodeHeightDirect}
\alias{setNodeHeightDirect,CytoscapeWindowClass-method}
\title{setNodeHeightDirect}
\description{
In the specified CytoscapeWindow, set the height of the specified node.
Not that the node dimensions (height and width) must be unlocked for
this to work.  If they ARE locked, then node and height change together,
as specified by a node size rule, or the setNodeSizeDirect method
}
\usage{
setNodeHeightDirect(obj, node.name, new.height)
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
  setNodeWidthDirect
  lockNodeDimensions
  setNodeSizeDirect
  setNodeHeightDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeHeightDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  lockNodeDimensions (cw, 'default', FALSE)
  setNodeHeightDirect (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
