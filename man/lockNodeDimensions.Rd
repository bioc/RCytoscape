\name{lockNodeDimensions}
\alias{lockNodeDimensions}
\alias{lockNodeDimensions,CytoscapeConnectionClass-method}
\title{lockNodeDimensions}
\description{
Select the specified nodes.
}
\usage{
lockNodeDimensions(obj, visual.style.name, new.state)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{visual.style.name}{a \code{string} object, naming the visual
    style whose 'locked' you wish to change}
  \item{new.state}{a \code{boolean} object, TRUE or FALSE}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setNodeSize
  setNodeWidth
  setNodeHeight
}

\examples{
  cw <- new.CytoscapeWindow ('lockNodeDimensions demo', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw)
  redraw (cw)
  lockNodeDimensions (cw, 'default', FALSE)
  setNodeWidth (cw, 'A', 100)
  setNodeHeight (cw, 'A', 50)

}

\keyword{graph}
