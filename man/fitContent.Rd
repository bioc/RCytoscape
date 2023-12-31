\name{fitContent}
\alias{fitContent}
\alias{fitContent,CytoscapeWindowClass-method}
\title{fitContent}
\description{
  Using all of the available window (the Cytoscape drawing canvas)
  display the current graph.
}
\usage{
fitContent(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setZoom
  fitSelectedContent
 }

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('fitContent.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setZoom (cw, 0.1)
  fitContent (cw)
  setZoom (cw, 10.0)
  fitContent (cw)
}}



\keyword{graph}
