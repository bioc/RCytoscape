\name{destroyAllWindows}
\alias{destroyAllWindows}
\alias{destroyAllWindows,CytoscapeWindowClass-method}
\title{destroyAllWindows}
\description{
Remove and delete all the network windows currently held by Cytoscape.
Note that though this is a CytoscapeWindowClass method, its affects
ALL currently active CytoscapeWindowClass windows, not just its own window.
}
\usage{
destroyAllWindows(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\examples{
  cw1 <- CytoscapeWindow ('cw1', makeSimpleGraph ())
  cw2 <- CytoscapeWindow ('cw2', makeSimpleGraph ())
  destroyAllWindows (cw1)
}

\keyword{graph}
