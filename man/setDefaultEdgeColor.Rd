\name{setDefaultEdgeColor}
\alias{setDefaultEdgeColor}
\alias{setDefaultEdgeColor,CytoscapeWindowClass-method}
\title{setDefaultEdgeColor}
\description{
In the specified CytoscapeWindow, stipulate the color for all edges
other than those mentioned in a edge color rule.
}
\usage{
setDefaultEdgeColor(obj, new.color, vizmap.style.name = "default")
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{new.color}{a \code{String} object, a hex string, of the form '#RRGGBB'.}
  \item{vizmap.style.name}{a \code{String} object, if this vizmap style
    needs to be distinguished from the default type.}
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
}

\examples{
  cw <- new.CytoscapeWindow ('sendDefaultEdgeColor.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layout (cw, 'jgraph-spring')
  setDefaultEdgeColor (cw, '#FFFFFF') # white edges
  redraw (cw)
}


\keyword{graph}
