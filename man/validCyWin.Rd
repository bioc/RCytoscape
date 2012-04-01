\name{validCyWin}
\alias{validCyWin}
\title{
validCyWin
}
\description{Checks all the slots of the object, printing error messages
  as needed, returning TRUE or FALSE.  In particular, RCytoscape imposes
  some requirements on graphs, including the use of an 'edgeType' edge
  attribute, and a call to initNodeAttribute and initEdgeAttribute for
  every attribute.  These requirements are checked here.
}
\usage{
validCyWin(obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{An instance of the CytoscapeWindow class}
}

\value{
TRUE or FALSE.  Error messages are written to stderr.
}
\author{
Paul Shannon
}

\examples{
\dontrun{
  g <- new ('graphNEL', edgemode='directed')
  cw <- new.CytoscapeWindow ('test validCyWin', graph=g)
  validCyWin (cw)  # should see warning about absence of the privileged edge attribute 'edgeType'; returns FALSE

}
}

\keyword{graph}%
