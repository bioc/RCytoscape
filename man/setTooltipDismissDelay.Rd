\name{setTooltipDismissDelay}
\alias{setTooltipDismissDelay}
\alias{setTooltipDismissDelay,CytoscapeConnectionClass-method}
\title{setTooltipDismissDelay}
\description{
Specify the number of milliseconds before the tooltip (a small
lightweight window) pops up over a node or edge.
}
\usage{
setTooltipDismissDelay(obj, msecs)
}
\arguments{
  \item{obj}{a \code{CytoscapeConnectionClass} object. }
  \item{msecs}{an \code{integer}.}
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  setTooltipInitialDelay,
  setTooltip,
  setNodeTooltipRule,
  setEdgeTooltipRule,
  setNodeTooltipDirect,
  setEdgeTooltipDirect
 }

\examples{
\donttest{
  window.title = 'setTooltipDismissDelay demo'
  cw <- new.CytoscapeWindow (window.title, graph=makeSimpleGraph())
    # use node type as the tooltip
  setNodeTooltipRule (cw, 'type')
    # and edgeType
  setEdgeTooltipRule (cw, 'edgeType')
  displayGraph (cw)
  redraw (cw)
  layoutNetwork(cw, 'jgraph-spring')
     # have the tooltips popup after 200 milliseconds, and then
     # disappear after 3000 (3 seconds)
  setTooltipInitialDelay (cw, 200)
  setTooltipDismissDelay (cw, 3000)
}}


\keyword{graph}
