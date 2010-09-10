\name{destroyWindow}
\alias{destroyWindow}
\alias{destroyWindow,CytoscapeWindowClass-method}
\title{destroyWindow}
\description{
Remove and delete Cytoscape's (copy of the) network and window for the
specified CytoscapeWindowClass object.
}
\usage{
destroyWindow(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
Nothing.
}
\author{Paul Shannon}
\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  destroyWindow (cw)
}

\keyword{graph}
