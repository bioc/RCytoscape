\name{CytoscapeConnection}
\alias{CytoscapeConnection}
\title{
CytoscapeConnection
}
\description{
The constructor for the CytoscapeConnectionClass.  This class is both
the base class for CytoscapeWindow objects, and quite usefully, and
instantiable object in its own right.   It is very useful for calling
the many RCytoscape methods which do not address a single window in
particular:  getWindowList, getWindowCount, deleteWindow,
getNodeShapes, etc.
}
\usage{
CytoscapeConnection (host = "localhost", rpcPort = 9000)
}

\arguments{
  \item{host}{
    Defaults to 'localhost', this is the domain name of a machine which
    is running Cytoscape with the appropriate XMLRPC server plugin.
}
  \item{rpcPort}{
    Defaults to 9000, this may be any port to which the CytoscapeRPC
    server is listening.
}
}
\value{
  An object of the CytoscapeConnection Class.
}
\author{
  Paul Shannon
}

\seealso{
  ping
  version
  msg
  clearMsg
  getWindowCount
  getWindowID
  getWindowCount
  getWindowList
  deleteWindow
  deleteAllWindows
  getNodeShapes
  getAttributeClassNames
  getLineStyles
  getArrowShapes
  getLayoutNames
  haveNodeAttribute
  haveEdgeAttribute
  getGraphFromCyWindow
  hidePanel
  dockPanel
  floatPanel
  }


\examples{
\donttest{
  cy <- CytoscapeConnection ()
  deleteAllWindows (cy)
  getNodeShapes (cy)
  hidePanel (cy, 'Control')
}}

\keyword{ graph }
