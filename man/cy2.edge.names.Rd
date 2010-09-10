\name{cy2.edge.names}
\alias{cy2.edge.names}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cy2.edge.names
}
\description{
Bioconductor graph edges are named, i.e., A~B.  The same edge in the
Cytoscape domain would be 'A (<edgeType>) B', where '<edgeType>' might
be 'phosphorylates' or 'represses'.
}
\usage{
cy2.edge.names(graph)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
}
}

\value{
A named list, in with Cytoscape edges names are the content, and bioc
graph edge names are their names.
}
\author{
Paul Shannon
}

\examples{
  g <- makeSimpleGraph ()
  cy2.edge.names (g)
  #                     A~B                      B~C                      C~A 
  #  "A (phosphorylates) B" "B (synthetic lethal) C"        "C (undefined) A" 
}

\keyword{graph}%
