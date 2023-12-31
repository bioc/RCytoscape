\name{getEdgeAttributeNames}

\alias{getEdgeAttributeNames}
\alias{getEdgeAttributeNames,CytoscapeConnectionClass-method}

\title{getEdgeAttributeNames}
\description{
Node and edge attributes belong to the Cytoscape session as a whole, not
to a particular window.  Use this method to find out the name of the
currently definededge attributes.

}
\usage{
getEdgeAttributeNames(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object or CytoscapeWindow
    object. }
}

\value{
A list of names.
}
\author{Paul Shannon}

\seealso{
  getEdgeAttribute
  deleteEdgeAttribute
  getNodeAttributeNames
}



\examples{
\donttest{

  cy = CytoscapeConnection ()
  print (getEdgeAttributeNames (cy))

}}


\keyword{graph}
