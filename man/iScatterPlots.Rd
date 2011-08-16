\name{iScatterPlots}
\alias{iScatterPlots}
\alias{iScatterPlots.default}
\title{Interactive 2D/3D Scatter Plot}
\description{Constructs an interactive two- as well as three-dimensional scatter plot. The users can alternate between 2D and 3D plots.}
\usage{
iScatterPlots(x, y, z = NULL, fileName, directory, ...)

\method{iScatterPlots}{default}(x, y, z = NULL, fileName, directory,
        fpng2d = "2Dplot", fpng3d = "3Dplot", dataPoints = NULL, ...)

}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot}
  \item{z}{the z coordinates of points in the plot (optional)}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng2d}{Character. The name of the image file that contains the 2D plot, without the extension of the image. (optional)}
  \item{fpng3d}{Character. The name of the image file that contains the 3D plot, without the extension of the image. (optional)}
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
  \code{\link{iScatter3D}}
}
\examples{

# Example 1
data(swiss)
iScatterPlots(swiss[,1], swiss[,2], swiss[,3], fileName="scatter2D3D",
  directory="scatterPlots_1", fpng2d="scatter2D", dataPoints=rownames(swiss),
  fpng3d="scatter3D", main="2D/3D plot", xlab=colnames(swiss[1]),
  ylab=colnames(swiss[2]), zlab=colnames(swiss[3]), bg="blue", pch=21,
  panel.first = grid(7,7,col = "gray"))

# Example 2
data(mtcars)
iScatterPlots(mtcars[,1], mtcars[,2], mtcars[,3], fileName="scatter2D3D",
    directory="scatterPlots_2", fpng2d="scatter2D", dataPoints=rownames(mtcars),
    fpng3d="scatterPlots_2", main="2D/3D plot", xlab=colnames(mtcars[1]),
    ylab=colnames(mtcars[2]), zlab=colnames(mtcars[3]), bg="blue", pch=21,
    panel.first = grid(7,7,col = "gray"))
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
