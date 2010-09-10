\name{layout}
\alias{layout}
\alias{layout,CytoscapeWindowClass-method}
\title{layout}
\description{
Layout the current graph according to the specified algorithm.
}
\usage{
layout(obj, layout.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{layout.name}{a \code{string}, one of the values returned by \code{getLayoutNames}. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  redraw (cw)  # applies default vizmap (rendering) rules, plus any you
               # have specified
}


\keyword{graph}
