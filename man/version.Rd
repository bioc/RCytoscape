\name{version}
\alias{version}
\alias{version,CytoscapeWindowClass-method}
\title{version}
\description{
Test the connection to Cytoscape.  
}
\usage{
version(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
  "A string describing the current version of the CytoscapeRPC plugin."
}

\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  print (version (cw))
    # e.g., "1.1.2 Paul"
}

\keyword{graph}
