\name{destroyAllWindows}
\alias{destroyAllWindows}
\alias{destroyAllWindows,CytoscapeConnectionClass-method}
\title{destroyAllWindows}
\description{
Remove and delete all the network windows currently held by Cytoscape.
}
\usage{
destroyAllWindows(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\examples{
  cy <- CytoscapeConnection ()
  destroyAllWindows (cy)
}

\keyword{graph}
