\name{getLineStyles}
\alias{getLineStyles}
\alias{getLineStyles,CytoscapeWindowClass-method}
\title{getLineStyles}
\description{
Retrieve the names of the currently supported line types -- values which
can be used to render edges, and thus can be used in calls to 'setEdgeLineStyleRule'
}
\usage{
getLineStyles(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings, e.g., 'SOLID', 'DOT'
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  getLineStyles (cw)
  #  [1] "SOLID" "LONG_DASH" "EQUAL_DASH" ...
}

\keyword{graph}
