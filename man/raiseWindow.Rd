\name{raiseWindow}
\alias{raiseWindow}
\alias{raiseWindow,CytoscapeWindowClass-method}
\title{raiseWindow}
\description{
Raise this window to the top on the Cytoscape desktop, so that it can be seen.
}
\usage{
raiseWindow(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  resizeWindow
}

\examples{
  cw <- new.CytoscapeWindow ('raiseWindow.test', graph=makeSimpleGraph())
  raiseWindow (cw)
}

\keyword{graph}
