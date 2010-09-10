\name{hidePanel}
\alias{hidePanel}
\alias{hidePanel,CytoscapeWindowClass-method}
\title{hidePanel}
\description{
The specified panel will be hidden, and no longer visible in the Cytoscape
Desktop of, if floating, elsewhere on the computer screen.  The
\code{panelName}  parameter is very flexible:  a match is defined as a
case-independent match of the supplied panelName to any starting
characters in the actual panelName.  Thus, 'd' and 'DA' both identify 'Data Panel'.
}
\usage{
hidePanel(obj, panelName)
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
  dockPanel
}


\examples{
  cw <- CytoscapeWindow ('test', graph=makeSimpleGraph())
   # or
  cw <- CytoscapeWindow ('headless', create.window=FALSE)
  hidePanel (cw, 'Control Panel')
   # or
  hidePanel (cw, 'c')
}


\keyword{graph}
