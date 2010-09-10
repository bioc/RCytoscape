\name{getArrowShapes}
\alias{getArrowShapes}
\alias{getArrowShapes,CytoscapeWindowClass-method}
\title{getArrowShapes}
\description{
Retrieve the names of the currently supported 'arrows' -- the
decorations can (optionally) appear at the ends of edges, adjacent to
the nodes they connect, and conveying information about the nature of
the nodes' relationship.
of strings.
}
\usage{
getArrowShapes(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings, e.g., 'WHITE_DIAMOND', 'BLACK_T'
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  getArrowShapes (cw)
    # [1] "No Arrow" "Diamond" "Delta" "Arrow" "T" "Circle" "Half Arrow Top"  "Half Arrow Bottom"
}

\keyword{graph}
