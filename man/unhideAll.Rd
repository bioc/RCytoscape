\name{unhideAll}
\alias{unhideAll}
\alias{unhideAll,CytoscapeWindowClass-method}
\title{unhideAll}
\description{
Currently (in Cytoscape 2.7) broken.  The redisplay of hidden nodes and
edges does not alwasy work...
}
\usage{
unhideAll(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  selectNodes
  clearSelection
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  redraw (cw)
  clearSelection (cw)
  selectNodes (cw, 'A')
  hideSelectedNodes (cw)
}


\keyword{graph}
