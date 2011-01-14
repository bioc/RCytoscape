\name{getPosition}
\alias{getPosition}
\alias{getPosition,CytoscapeWindowClass-method}
\title{getPosition}
\description{
  Get the position of the specified nodes on the CytoscapeWindow
  canvas.  Useful in retrieving the current position of nodes in the window.
}
\usage{
getPosition(obj, node.names)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{list} of strings, the names of nodes to select. }
}

\value{
A names list of x,y pairs; names are the identifiers of nodes supplied
when the graph was created.
}
\author{Paul Shannon}

\examples{
  cw <- new.CytoscapeWindow ('getPosition.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw)
  getPosition (cw, c ('A', 'B', 'C'))
}

\keyword{graph}
