\name{setNodeColorRule}
\alias{setNodeColorRule}
\alias{setNodeColorRule,CytoscapeWindowClass-method}
\title{setNodeColorRule}
\description{
Specify how data attributes -- for the specified named attribute -- is
mapped to node color.
}

\usage{
setNodeColorRule(obj, node.attribute.name, control.points, colors)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will, when this
rule is applied, determine the color of each node.}
  \item{control.points}{a list of (currently, only 3) values.  Wise
choices are: the minimum, the maximum, some sensible midpoint.}
  \item{colors}{a list of (currently) only 3 colors, expressed as
hexadecimal RGB, like this:  '#FF0000' or '#FA8800'}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeShapeRule
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  molecule.types <- c ("kinase", "transcription factor", "glycoprotein")
  node.shapes <- c ("trapezoid", "round_rect", "ellipse")
  setNodeShapeRule (cw, node.attribute.name='type', molecule.types,  node.shapes)
  redraw (cw)
}


\keyword{graph}
