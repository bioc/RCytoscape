\name{getWindowList}
\alias{getWindowList}
\alias{getWindowList,CytoscapeWindowClass-method}
\title{getWindowList}
\description{
Returns a named list of windows in the current Cytoscape Desktop. 
}
\usage{
getWindowList(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A named list,  in which the values are the titles of the windows; the names of the list are integers.
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('toast', graph=makeSimpleGraph())
  getWindowList (cw)   # assuming 'test' already exists, you will see:
    #    39     38 
    # "test" "toast" 
}

\keyword{graph}
