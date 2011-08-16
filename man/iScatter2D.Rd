\name{iScatter2D}
\alias{iScatter2D}
\alias{iScatter2D.default}
\title{Interactive 2D Scatter Plot}
\description{Constructs an interactive two-dimensional (2D) graph (scatter plot) in an HTML wrapper.}
\usage{
iScatter2D(x, y, fileName, directory, ...)

\method{iScatter2D}{default}(x, y = NULL, fileName, directory, fpng2d = "2Dplot",
          dataPoints = NULL, ...)
}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng2d}{Character. The name of the image file that contains the plot, without the extension of the image. (optional)}
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
  \code{\link{iScatter3D}},
  \code{\link{iScatterPlots}}
}
\examples{

# Example 1
data(swiss)
iScatter2D(swiss[,1], swiss[,2], fileName="scatter2D", directory="scatter2D_1", 
  fpng2d="image2D", dataPoints=rownames(swiss), main="Two-dimensional plot",
  xlab=colnames(swiss[1]), ylab=colnames(swiss[2]), bg="blue", pch=21,
  panel.first = grid(7,7,col = "gray") )

# Example 2
data(cars)
iScatter2D(cars[,1], cars[,2], fileName="scatter2D", directory="scatter2D_2", 
  fpng2d="image2D", dataPoints=rownames(cars), main="Two-dimensional plot",
  xlab=colnames(cars[1]), ylab=colnames(cars[2]), bg="blue", pch=21,
  panel.first = grid(7,7,col = "gray") )

# Example 3
data(mtcars)
iScatter2D(mtcars[,1], mtcars[,2], fileName="scatter2D", directory="scatter2D_3",
  fpng2d="image2D", main="Two-dimensional plot", dataPoints=rownames(mtcars),
  xlab=colnames(mtcars[1]), ylab=colnames(mtcars[2]), bg="blue", pch=21,
  panel.first = grid(7,7,col = "gray"))

}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
