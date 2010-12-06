\name{destroyWindow}
\alias{destroyWindow}
\alias{destroyWindow,CytoscapeConnectionClass-method}
\title{destroyWindow}
\description{
Remove and delete Cytoscape's (copy of the) network and window for the
specified CytoscapeConnectionClass object.
}
\usage{
destroyWindow(obj, windowTitle)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{windowTitle}{a \code{character} string. }
}

\value{
Nothing.
}
\author{Paul Shannon}
\examples{
  cy <- CytoscapeConnection ()
  window.title <- 'demo destroyWindow'
  cw <- CytoscapeWindow (window.title, graph=makeSimpleGraph())
  destroyWindow (cw, window.title)
}

\keyword{graph}
