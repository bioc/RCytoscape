\name{setNodeSizeRule}
\alias{setNodeSizeRule}
\alias{setNodeSizeRule,CytoscapeWindowClass-method}
\title{setNodeSizeRule}
\description{
Specify how data attributes how the specified node attribute values
determine the node size.}
\usage{
setNodeSizeRule (obj, node.attribute.name, attribute.values,node.sizes)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node  attribute whose values will, when this
rule is applied, determine the size of each node.}
  \item{attribute.values}{A list of (currently, exactly 3) values, which
  specify the 'control points' to control the coloring of nodes}
  \item{node.sizes}{The nodes sizes which correspond to the control points.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeColorRule
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  control.points <- c (2, 30, 100)
  sizes          <- c (20, 50, 100)
  node.attribute.name <- 'count'   # previously defined, has values which
                                  # range between 2 and 100
  setNodeSizeRule (cw, 'count', control.points, sizes)
  redraw (cw)
}

\keyword{graph}

