\name{ping}
\alias{ping}
\alias{ping,CytoscapeWindowClass-method}
\title{ping}
\description{
Test the connection to Cytoscape.  
}
\usage{
ping(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
  "It works!"
}

\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  ping (cw)
    # "It works!"   
}

\keyword{graph}
