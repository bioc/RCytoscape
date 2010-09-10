\name{getNodeShapes}
\alias{getNodeShapes}
\alias{getNodeShapes,CytoscapeWindowClass-method}
\title{getNodeShapes}
\description{
Retrieve the names of the currently supported node shapes.
}
\usage{
getNodeShapes(obj)
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
  getNodeShapes(cw)
   #  [1] "trapezoid"     "round_rect"    "ellipse"
}

\keyword{graph}
