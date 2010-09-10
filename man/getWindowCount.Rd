\name{getWindowCount}
\alias{getWindowCount}
\alias{getWindowCount,CytoscapeWindowClass-method}
\title{getWindowCount}
\description{
Returns the number of windows which currently exist in the Cytoscape Desktop.
}
\usage{
getWindowCount(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
An integer.
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  count.at.start = getWindowCount (cw)
  cw2 <- CytoscapeWindow ('test1', graph=makeSimpleGraph())
  cw3 <- CytoscapeWindow ('test2', graph=makeSimpleGraph())
  getWindowCount (cw)
    # should be two greater than 'count.at.start'
}


\keyword{graph}
