\name{setNodeBorderColorDirect}
\alias{setNodeBorderColorDirect}
\alias{setNodeBorderColorDirect,CytoscapeWindowClass-method}
\title{setNodeBorderColorDirect}
\description{
In the specified CytoscapeWindow, set the color of the border of the specified node.
}
\usage{
setNodeBorderColorDirect(obj, node.names, new.color)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.color}{a \code{String} object, in standard hex notation.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeSizeDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeBorderColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setNodeBorderColorDirect (cw, 'A', '#FFFF00')
  redraw (cw)
}


\keyword{graph}
