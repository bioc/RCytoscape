\name{saveImage}
\alias{saveImage}
\alias{saveImage,CytoscapeWindowClass-method}
\title{saveImage}
\description{
Write an image of the specified type to the specified file, at the
specified scaling factor.  
}
\usage{
saveImage(obj, file.name, image.type, scale)
}
\arguments{

  \item{obj}{a \code{CytoscapeWindowClass} object. }
  \item{file.name}{a \code{char} object. }
  \item{image.type}{a \code{char} object. 'jpeg' is the only image type   currently supported}
  \item{scale}{a \code{numeric} object.  How large (or small) to  scale the image. }

}

\value{
None.
}
\author{Paul Shannon}

\seealso{
  selectNodes
  clearSelection
}

\examples{
  cw <- new.CytoscapeWindow ('saveImage.test', graph=makeSimpleGraph())
  displayGraph (cw)
  layout (cw, 'jgraph-spring')
  redraw (cw)
  filename = tempfile () # paste (getwd (), 'saveImageTest.jpg', sep='/')
  # saveImage (cw, filename, 'jpeg', 2.0)   # doesn't work reliably at bioc
}


\keyword{graph}
