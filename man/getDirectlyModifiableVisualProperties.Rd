\name{getDirectlyModifiableVisualProperties}
\alias{getDirectlyModifiableVisualProperties}
\alias{getDirectlyModifiableVisualProperties,CytoscapeConnectionClass-method}
\title{getDirectlyModifiableVisualProperties}
\description{
Retrieve the names of those visual attributes which can be set directly,
bypassing vizmap rules.
}
\usage{
getDirectlyModifiableVisualProperties(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
}

\value{
A list of about 60 character strings, e.g., "Node Color" and "Edge Font Size"
}
\author{Paul Shannon}

\examples{
  cy <- CytoscapeConnection ()
  getDirectlyModifiableVisualProperties (cy)
   #  [1] "Node Color"                      "Node Border Color"               "Node Line Type"                  "Node Shape"                     
   #  [5] "Node Size"                       "Node Width"                      "Node Height"                     "Node Label"                     
   # ...

}

\keyword{graph}
