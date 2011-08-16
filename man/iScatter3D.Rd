\name{iScatter3D}
\alias{iScatter3D}
\alias{iScatter3D.default}
\title{Interactive 3D Scatter Plot}
\description{Constructs an interactive three-dimensional (3D) graph (scatter plot) in an HTML wrapper.}
\usage{
iScatter3D(x, y, z, fileName, directory, ...)

\method{iScatter3D}{default}(x, y = NULL, z = NULL, fileName, directory,
          fpng3d = "3Dplot", dataPoints = NULL, ...)

}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot (optional)}
  \item{z}{the z coordinates of points in the plot (optional)}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng3d}{Character. The name of the image file that contains the plot, without the extension of the image. (optional)}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y. (optional)}
  \item{\dots}{More graphical parameters can be given as arguments (See \code{\link{par}}).}
}
\references{
  \url{http://www.cranfield.ac.uk/health/researchareas/bioinformatics/page53292.html}
}
\author{
Eleni Chatzimichali <e.chatzimichali@cranfield.ac.uk>,
Conrad Bessant <c.bessant@cranfield.ac.uk>
}
\note{
The function creates a new folder, which contains the HTML file, the PNG image as well as other necessary scripts.
}
\seealso{
  \code{\link{iScatter2D}},
  \code{\link{iScatterPlots}}
}
\examples{
# Example 1
data(swiss)
iScatter3D(swiss[,1], swiss[,2], swiss[,3], fileName="scatter3D",
  directory="scatter3D_1", fpng3d="image3D", dataPoints=rownames(swiss),
  main="Three-dimensional plot", xlab=colnames(swiss[1]),
  ylab=colnames(swiss[2]), zlab=colnames(swiss[3]), grid=TRUE,
  pch=21, bg="blue")
  
# Example 2
data(mtcars)
iScatter3D(mtcars[,1], mtcars[,2], mtcars[,3], fileName="scatter3D",
  directory="scatter3D_2", fpng3d="image3D", dataPoints=rownames(mtcars),
   main="Three-dimensional plot", xlab=colnames(mtcars[1]),
   ylab=colnames(mtcars[2]), zlab=colnames(mtcars[3]), grid=TRUE,
   pch=21, bg="blue")
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
