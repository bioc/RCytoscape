\name{setNodeColorDirect}
\alias{setNodeColorDirect}
\alias{setNodeColorDirect,CytoscapeWindowClass-method}
\title{setNodeColorDirect}
\description{
In the specified CytoscapeWindow, set the color of the specified node or
nodes.   This method bypasses the vizmap, and excludes this node, for
the duration of the current Cytoscape session, from further manipulation by
vizmap color rules.
}
\usage{
setNodeColorDirect(obj, node.names, new.color)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{a \code{String} list object.}
  \item{new.color}{an \code{String} object, using the standard
    hexadecimal form, eg, '#FF88AA'}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeColorRule
 }

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('setNodeColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setNodeColorDirect (cw, 'A', '#880000')
  redraw (cw)
}}



\keyword{graph}
