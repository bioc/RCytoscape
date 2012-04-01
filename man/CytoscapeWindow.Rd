\name{CytoscapeWindow}
\alias{CytoscapeWindow}
\title{
CytoscapeWindow
}
\description{
The constructor for the CytoscapeWindowClass
}
\usage{
CytoscapeWindow(title, graph = new("graphNEL", edgemode='directed'), host = "localhost", rpcPort = 9000, create.window = TRUE)
}

\arguments{
  \item{title}{
    A character string, this is the name you will see on the Cytoscape
    network window.  Multiple windows with the same name are not permitted.
}
  \item{graph}{
    A Bioconductor graph. 
}
  \item{host}{
    Defaults to 'localhost', this is the domain name of a machine which
    is running Cytoscape with the appropriate XMLRPC server plugin.
}
  \item{rpcPort}{
    Defaults to 9000, this may be any port to which the CytoscapeRPC
    server is listening.
}
  \item{create.window}{
     Defaults to TRUE, but if you wish to defer creating a window, set
  this argument to FALSE.
}
}
\value{
  An object of the CytoscapeWindow Class.
}
\author{
  Paul Shannon
}

\seealso{
  new.CytoscapeWindow
  existing.CytoscapeWindow
  }

\examples{
\dontrun{
  cw <- CytoscapeWindow ('cw demo', new ('graphNEL'))
}
}

\keyword{ graph }
