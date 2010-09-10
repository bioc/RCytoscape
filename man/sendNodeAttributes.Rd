\name{sendNodeAttributes}
\alias{sendNodeAttributes}
\alias{sendNodeAttributes,CytoscapeWindowClass-method}
\title{sendNodeAttributes}
\description{
Transfer the named node attribute from the the R graph (found in
obj@graph) to Cytoscape. This method is typically called by
displayGraph, which will suffice for most users' needs.  It transfers
the specified node attributes, for all nodes, from the cw@graph slot to Cytoscape.
}
\usage{
sendNodeAttributes(obj, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{a \code{string} one of the attributes defined on
    the nodes. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  sendNodeAttributesDirect
  sendEdgeAttributes
  sendEdgeAttributesDirect
  sendEdges
  sendNodes
  displayGraph
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  attribute.names = noa.names (cw@graph)

  for (attribute.name in attribute.names)
    result = sendNodeAttributes (cw, attribute.name)

}


\keyword{graph}
