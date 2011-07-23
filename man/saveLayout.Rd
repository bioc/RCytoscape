\name{saveLayout}
\alias{saveLayout}
\alias{saveLayout,CytoscapeWindowClass-method}
\title{saveLayout}
\description{
save the current layout (that is, node positions) to the specified file.
}
\usage{
saveLayout(obj, filename)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{filename}{a \code{string}}
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  restoreLayout
}

\examples{
  cw <- new.CytoscapeWindow ('saveLayout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  saveLayout (cw, 'layout.RData')
  layout (cw, 'jgraph-circle')
  restoreLayout (cw, 'layout.RData')
}


\keyword{graph}
