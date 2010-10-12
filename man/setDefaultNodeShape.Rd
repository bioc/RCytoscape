\name{setDefaultNodeShape}
\alias{setDefaultNodeShape}
\alias{setDefaultNodeShape,CytoscapeWindowClass-method}
\title{setDefaultNodeShape}
\description{
In the specified CytoscapeWindow, stipulate the color for all nodeBorders
other than those mentioned in a node border color rule.
}
\usage{
setDefaultNodeShape(obj, new.shape, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.shape}{a \code{String} object, one of the permissible values
    (see getNodeShapes).}
  \item{vizmap.style.name}{a \code{String} object.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
   getNodeShapes
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
  legal.shapes <- getNodeShapes (cw)
  # stopifnot ('diamond' %in% legal.shapes)
  setDefaultNodeShape (cw, 'diamond')
  redraw (cw)
}


\keyword{graph}
