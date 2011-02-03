\name{resizeWindow}
\alias{resizeWindow}
\alias{resizeWindow,CytoscapeWindowClass-method}
\title{resizeWindow}
\description{
  Change the dimensions of this window
}
\usage{
resizeWindow(obj, width, height)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{width}{an \code{integer} in pixels. }
  \item{height}{an \code{integer} in pixels. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  raiseWindow
}

\examples{
  cw <- new.CytoscapeWindow ('resizeWindow.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw)
  resizeWindow (cw, 400, 400)
}

\keyword{graph}
