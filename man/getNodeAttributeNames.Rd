\name{getNodeAttributeNames}

\alias{getNodeAttributeNames}
\alias{getNodeAttributeNames,CytoscapeConnectionClass-method}

\title{getNodeAttributeNames}
\description{
Node and node attributes belong to the Cytoscape session as a whole, not
to a particular window.  Use this method to find out the name of the
currently defined node attributes.

}
\usage{
getNodeAttributeNames(obj)
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
  getNodeAttribute
  deleteNodeAttribute
  getEdgeAttributeNames
}



\examples{
\donttest{

  cy = CytoscapeConnection ()
  print (getNodeAttributeNames (cy))

}}


\keyword{graph}
