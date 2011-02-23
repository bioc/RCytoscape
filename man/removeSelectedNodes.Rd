\name{removeSelectedNodes}
\alias{removeSelectedNodes}
\alias{removeSelectedNodes,CytoscapeWindowClass-method}
\title{removeSelectedNodes}
\description{
Remove (destroy, delete) the currently selected nodes.
}
\usage{
removeSelectedNodes(obj, remove.from.root.graph.also=TRUE)

}
\arguments{
  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{remove.from.root.graph.also}{a \code{boolean} object, default TRUE }
}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  clearSelection
  invertNodeSelection
}

\examples{
  cw <- new.CytoscapeWindow ('removeSelectedNodes demo', graph=makeSimpleGraph())
     # all nodes should be selected, since none were before.
     # then all will be removed
  invertNodeSelection (cw)
  removeSelectedNodes (cw)
  redraw (cw)
     # a richer test will be to programmatically select nodes, but that
     #  does not work yet (pshannon, 13 jan 2011)
   
}

\keyword{graph}
