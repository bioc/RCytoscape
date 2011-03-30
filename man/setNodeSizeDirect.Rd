\name{setNodeSizeDirect}
\alias{setNodeSizeDirect}
\alias{setNodeSizeDirect,CytoscapeWindowClass-method}
\title{setNodeSizeDirect}
\description{
In the specified CytoscapeWindow, set the size of the specified node.
Not that the node dimensions (size and size) must be locked (the default
state) for this to work.  Node height and width change together.
}
\usage{
setNodeSizeDirect(obj, node.names, new.size)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.size}{an \code{integer}, in pixel units.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  lockNodeDimensions
  setNodeWidthDirect
  setNodeHeightDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeSizeDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeSizeDirect (cw, 'A', 32)
  redraw (cw)
}


\keyword{graph}
