\name{setEdgeColorRule}
\alias{setEdgeColorRule}
\alias{setEdgeColorRule,CytoscapeWindowClass-method}
\title{setEdgeColorRule}
\description{
Specify how data attributes -- for the specified named attribute -- is
mapped to edge color.}

\usage{
setEdgeColorRule(obj, attribute.name, attribute.values, colors, default.color='#000000')
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{the edge attribute whose values will, when this
rule is applied, determine the color of each edge.}
  \item{attribute.values}{a list of (currently, only 3) values.  Wise
choices are: the minimum, the maximum, some sensible midpoint.}
  \item{colors}{a list of (currently) only 3 colors, expressed as
hexadecimal RGB, like this:  '#FF0000' or '#FA8800'}
  \item{default.color}{a \code{String} object, expressed in 
hexadecimal RGB, like this:  '#FF0000' or '#FA8800'}
}

\value{
None.
}
\author{Paul Shannon}
\seealso{
  setEdgeLineStyleRule
  setNodeColorRule
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  edgeType.values = c ('phosphorylates', 'synthetic lethal', 'undefined')
  colors = c ('#FF0000', '#FFFF00', '#00FF00')
  setEdgeColorRule (cw, 'edgeType',  edgeType.values, colors)
  redraw (cw)
}

\keyword{graph}
