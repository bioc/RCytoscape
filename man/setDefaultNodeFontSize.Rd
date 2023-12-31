\name{setDefaultNodeFontSize}
\alias{setDefaultNodeFontSize}
\alias{setDefaultNodeFontSize,CytoscapeConnectionClass-method}
\title{setDefaultNodeFontSize}
\description{
In the specified CytoscapeWindow, stipulate the color for all nodeBorders
other than those mentioned in a node border color rule.
}
\usage{
setDefaultNodeFontSize(obj, new.size, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.size}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
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
\donttest{
  cw <- new.CytoscapeWindow ('setDefaultNodeFontSize.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setDefaultNodeFontSize (cw, 32)
  redraw (cw)
}}



\keyword{graph}
