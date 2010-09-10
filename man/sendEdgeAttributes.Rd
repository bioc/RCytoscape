\name{sendEdgeAttributes}
\alias{sendEdgeAttributes}
\alias{sendEdgeAttributes,CytoscapeWindowClass-method}
\title{sendEdgeAttributes}
\description{
Transfer the named edge attribute from the the R graph (found in
obj@graph) to Cytoscape.  This method is typically called by
displayGraph, which will suffice for most users' needs.  It transfers
the specified edge attributes, for all edges, from the cw@graph slot to Cytoscape.
}
\usage{
sendEdgeAttributes(obj, attribute.name)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{attribute.name}{a \code{string} one of the attributes defined on
    the edges. }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  sendEdgeAttributesDirect
  sendNodeAttributes
  sendNodeAttributesDirect
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  attribute.names = eda.names (cw@graph)

  for (attribute.name in attribute.names)
    result = sendEdgeAttributes (cw, attribute.name)

}


\keyword{graph}
