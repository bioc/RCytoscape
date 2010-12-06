\name{setDefaultNodeColor}
\alias{setDefaultNodeColor}
\alias{setDefaultNodeColor,CytoscapeWindowClass-method}
\title{setDefaultNodeColor}
\description{
In the specified CytoscapeWindow, stipulate the color for all nodes
other than those mentioned in a node border color rule.
}
\usage{
setDefaultNodeColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
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
  cw <- new.CytoscapeWindow ('setDefaultNodeColor.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setDefaultNodeColor (cw, '#8888FF') # light blue
  redraw (cw)
}


\keyword{graph}
