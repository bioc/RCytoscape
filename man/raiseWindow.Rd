\name{raiseWindow}
\alias{raiseWindow}
\alias{raiseWindow,CytoscapeConnectionClass-method}
\title{raiseWindow}
\description{
Raise this window to the top on the Cytoscape desktop, so that it can be seen.
}
\usage{
raiseWindow(obj, window.title=NA)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object, or its subclass, \code{CytoscapeWindowClass}. }
  \item{window.title}{a \code{string}. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  resizeWindow
}

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('raiseWindow.test', graph=makeSimpleGraph())
  raiseWindow (cw)
}}


\keyword{graph}
