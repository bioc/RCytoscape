\name{setDefaultNodeColor}
\alias{setDefaultNodeColor}
\alias{setDefaultNodeColor,CytoscapeConnectionClass-method}
\title{setDefaultNodeColor}
\description{
In the specified CytoscapeWindow, stipulate the color for all nodes
other than those mentioned in a node border color rule.
}
\usage{
setDefaultNodeColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{new.color}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
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
  cw <- new.CytoscapeWindow ('setDefaultNodeColor.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setDefaultNodeColor (cw, '#8888FF') # light blue
  redraw (cw)
}}



\keyword{graph}
