\name{CytoscapeWindowClass-class}
\docType{class}
\alias{CytoscapeWindowClass-class}
\title{Class "CytoscapeWindowClass" }
\description{A class providing access to the Cytoscape application. }
\section{Slots}{
  \describe{
    \item{\code{title}:}{An \code{attrData} the name of the window.}
    \item{\code{window.id}:}{An \code{attrData} Cytoscape's identifier.}
    \item{\code{graph}:}{An \code{attrData} a graph instance.}
    \item{\code{collectTimings}:}{An \code{logical} object.}
    \item{\code{uri}:}{An \code{attrData} the address of the Cytoscape
      XMLRPC server.}
  }
}

\section{Methods}{
  \describe{
    \item{createWindow}{}
    \item{destroyWindow}{}
    \item{destroyAllWindows}{}
    \item{displayGraph}{}
    \item{firstNeighbors}{}
    \item{getArrowShapes}{}
    \item{getLayoutNames}{}
    \item{getLineStyles}{}
    \item{getNodeShapes}{}
    \item{getWindowCount}{}
  }
}

\author{Paul Shannon}

\examples{
\donttest{
     # create a CytoscapeWindowClass object by calling the constructor
  c2 <- CytoscapeWindow ('cwc demo', makeSimpleGraph ())  

}}

\keyword{graphs}
\keyword{classes}

