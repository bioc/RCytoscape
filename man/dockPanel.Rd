\name{dockPanel}
\alias{dockPanel}
\alias{dockPanel,CytoscapeWindowClass-method}
\title{dockPanel}
\description{
The specified panel is returned to its 'home' position in the Cytoscape
Desktop if it had been previously floating or hidden.  The
\code{panelName}  parameter is very flexible:  a match is defined as a
case-independent match of the supplied panelName to any starting
characters in the actual panelName.  Thus, 'd' and 'DA' both identify 'Data Panel'.
}
\usage{
dockPanel(obj, panelName)
}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{panelName}{a character string, providing a partial or complete
    case-independent match to the start of the name of an actual panel. }
}

\value{
Nothing.
}
\author{Paul Shannon}


\seealso{
  floatPanel
  hidePanel
}

\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
   # or
  cw <- CytoscapeWindow ('headless', create.window=FALSE)
  dockPanel (cw, 'Control Panel')
   # or
  dockPanel (cw, 'c')
}

\keyword{graph}
