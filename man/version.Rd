\name{version}
\alias{version}
\alias{version,CytoscapeConnectionClass-method}
\title{version}
\description{
Test the connection to Cytoscape.  
}
\usage{
version(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
  "A string describing the current version of the CytoscapeRPC plugin."
}

\author{Paul Shannon}

\examples{
  cy <- CytoscapeConnection ()
  print (version (cy))
    # e.g., "1.3.2"
}

\keyword{graph}
