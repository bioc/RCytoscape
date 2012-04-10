\name{setNodeLabelColorDirect}
\alias{setNodeLabelColorDirect}
\alias{setNodeLabelColorDirect,CytoscapeWindowClass-method}
\title{setNodeLabelColorDirect}
\description{
In the specified CytoscapeWindow, set the size of the font used in
rendering the label of the specified node.
}
\usage{
setNodeLabelColorDirect(obj, node.names, new.colors)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.names}{one or more \code{String} objects.}
  \item{new.colors}{an \code{string}, using standard hex notation.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeFontSizeDirect
 }

\examples{
  cw <- new.CytoscapeWindow ('setNodeLabelColorDirect.test', graph=makeSimpleGraph())
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
  setNodeFontSizeDirect (cw, 'A', 50)
  setNodeLabelColorDirect (cw, 'A', '#FFFF00')
  redraw (cw)
}


\keyword{graph}
