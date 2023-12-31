\name{saveLayout}
\alias{saveLayout}
\alias{saveLayout,CytoscapeWindowClass-method}
\title{saveLayout}
\description{
save the current layout (that is, node positions) to the specified file.
}
\usage{
saveLayout(obj, filename, timestamp.in.filename=FALSE)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{filename}{a \code{string}.}
  \item{timestamp.in.filename}{\code{logical}.}
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  restoreLayout
}

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('saveLayout.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  saveLayout (cw, 'layout.RData')
  layoutNetwork(cw, 'jgraph-circle')
  restoreLayout (cw, 'layout.RData')
  saveLayout (cw, 'layout2', timestamp.in.filename=TRUE)
}}



\keyword{graph}
