\name{setNodeTooltipRule}
\alias{setNodeTooltipRule}
\alias{setNodeTooltipRule,CytoscapeWindowClass-method}
\title{setNodeTooltipRule}
\description{
Specify the node attribute to be used as the tooltip for each node.
Non-character attributes are converted to strings before they are used
as tooltips.}

\usage{
setNodeTooltipRule(obj, node.attribute.name)
}

\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{node.attribute.name}{the node attribute whose values will, when this
        rule is applied, determine the tooltip on each node.}
}

\value{
None.
}
\author{Paul Shannon}

\examples{
\donttest{
  cw <- new.CytoscapeWindow ('setNodeTooltipRule.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layoutNetwork(cw, 'jgraph-spring')
  redraw (cw)
  setNodeTooltipRule (cw, 'type')
  setNodeTooltipRule (cw, 'lfc')
  setNodeTooltipRule (cw, 'count')

}}


\keyword{graph}
