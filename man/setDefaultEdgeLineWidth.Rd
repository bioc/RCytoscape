\name{setDefaultEdgeLineWidth}
\alias{setDefaultEdgeLineWidth}
\alias{setDefaultEdgeLineWidth,CytoscapeWindowClass-method}
\title{setDefaultEdgeLineWidth}
\description{
In the specified CytoscapeWindow, stipulate the line width, in pixels for all edges.
}
\usage{
setDefaultEdgeLineWidth(obj, new.width, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.width}{an \code{integer} object, typically from 0 to 5.}
  \item{vizmap.style.name}{a \code{String} object.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
   setDefaultNodeShape
   setDefaultNodeColor
   setDefaultNodeSize
   setDefaultNodeColor
   setDefaultNodeBorderColor
   setDefaultNodeBorderWidth
   setDefaultNodeFontSize
   setDefaultNodeLabelColor
   setEdgeColorRule
}

\examples{
  cw <- new.CytoscapeWindow ('setDefaultEdgeLineWidth.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setDefaultEdgeLineWidth (cw, 5) 
  redraw (cw)
}


\keyword{graph}
