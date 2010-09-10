\name{getLayoutNames}
\alias{getLayoutNames}
\alias{getLayoutNames,CytoscapeWindowClass-method}
\title{getLayoutNames}
\description{
Retrieve the names of the currently supported layout algorithms.  These
may be used in subsequent calls to the 'layout' function.  Note that
some of the more attractive layout options, from yFiles, cannot be
run except from the user interface; their names do not appear here.
}
\usage{
getLayoutNames(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A list of character strings, e.g., "jgraph-circle" "attribute-circle" "jgraph-annealing"
}

\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  getLayoutNames (cw)
   #  [1] "jgraph-circle" "attribute-circle" "jgraph-annealing"  ...

}

\keyword{graph}
