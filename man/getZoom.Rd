\name{getZoom}
\alias{getZoom}
\alias{getZoom,CytoscapeWindowClass-method}
\title{getZoom}
\description{
  This method returns the zoom level of the CytoscapeWindow.  
  A value of 1.0 typically renders the graph with an ample margin.  A
  call to fitContent produces a zoom level of about 1.5.
}
\usage{
getZoom(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
A names list, x and y.
}
\author{Paul Shannon}

\seealso{
  setZoom
  getCenter
  setCenter
  getViewCoordinates
  fitContent
 }

\examples{
\donttest{
  window.title = 'getZoom demo'
  cw <- new.CytoscapeWindow (window.title, graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  print (getZoom (cw))
}}


\keyword{graph}
