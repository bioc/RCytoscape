\name{setNodeLabelDirect}
\alias{setNodeLabelDirect}
\alias{setNodeLabelDirect,CytoscapeWindowClass-method}
\title{setNodeLabelDirect}
\description{
In the specified CytoscapeWindow, set the labels of the specified nodes.
}
\usage{
setNodeLabelDirect(obj, node.names, new.labels)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.labels}{one or more \code{String} objects. If just one, then
    this is replicated for each of the supplied node.names.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeShapeDirect
 }

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('setNodeLabelDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setNodeLabelDirect (cw, 'A', 'A new, very long label')
  redraw (cw)
}}



\keyword{graph}
