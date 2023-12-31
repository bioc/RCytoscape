\name{setDefaultNodeShape}
\alias{setDefaultNodeShape}
\alias{setDefaultNodeShape,CytoscapeConnectionClass-method}
\title{setDefaultNodeShape}
\description{
For all CytoscapeWindow's, specify the default node shape.
}
\usage{
setDefaultNodeShape(obj, new.shape, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
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
\donttest{
  cw <- new.CytoscapeWindow ('setDefaultNodeShape.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  legal.shapes <- getNodeShapes (cw)
  # stopifnot ('diamond' %in% legal.shapes)
  setDefaultNodeShape (cw, 'diamond')
  redraw (cw)
}}



\keyword{graph}
