\name{clearMsg}
\alias{clearMsg}
\alias{clearMsg,CytoscapeWindowClass-method}
\title{clearMsg}
\description{
Clears any current message in the Cytoscape Desktop status bar.
}
\usage{
clearMsg(obj)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
}

\value{
Nothing.
}
\author{Paul Shannon}

\seealso{
  msg
}



\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
  displayGraph (cw)
  clearMsg (cw)
}


\keyword{graph}
