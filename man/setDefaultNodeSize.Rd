\name{setDefaultNodeSize}
\alias{setDefaultNodeSize}
\alias{setDefaultNodeSize,CytoscapeWindowClass-method}
\title{setDefaultNodeSize}
\description{
In the specified CytoscapeWindow, stipulate the color for all nodeBorders
other than those mentioned in a node border color rule.
}
\usage{
setDefaultNodeSize(obj, new.size, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.size}{a \code{integer} object, typically 20 to 100.}
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
   setDefaultEdgeLineWidth
   setEdgeColorRule
   setNodeBorderColorRule
 }

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setDefaultNodeSize (cw, 60) # an intermediate value
  redraw (cw)
}


\keyword{graph}
