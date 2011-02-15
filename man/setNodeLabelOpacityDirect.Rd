\name{setNodeLabelOpacityDirect}
\alias{setNodeLabelOpacityDirect}
\alias{setNodeLabelOpacityDirect,CytoscapeWindowClass-method}
\title{setNodeLabelOpacityDirect}
\description{
In the specified CytoscapeWindow, set the opacity of the label of the specified node.
}
\usage{
setNodeLabelOpacityDirect(obj, node.name, new.value)
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


\seealso{
  setNodeFillOpacityDirect
  setNodeOpacityDirect
  setNodeBorderOpacityDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeLabelOpacityDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setNodeLabelOpacityDirect (cw, 'A', 220)
  redraw (cw)
}


\keyword{graph}
