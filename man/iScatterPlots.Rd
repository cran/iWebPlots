\name{iScatterPlots}
\alias{iScatterPlots}
\title{
Interactive 2D/3D Scatter Plot
}
\description{
Constructs an interactive two- as well as three-dimensional scatter plot. The users can alternate between 2D and 3D plots.
}
\usage{
iScatterPlots(x, y, z = NULL, dataPoints, fileName, fileTitle, fpng2d, fpng3d,
    plotTitle = NULL, xlab = NULL, ylab = NULL, zlab = NULL)
}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot}
  \item{z}{the z coordinates of points in the plot}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y.}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{fileTitle}{Character. The title for the generated HTML page.}
  \item{fpng2d}{Character. The name of the 2D image (without the extension of the image).}
  \item{fpng3d}{Character. The name of the 3D image (without the extension of the image).}
  \item{plotTitle}{Character. The name of an overall title for the plot (optional).}
  \item{xlab}{Character. The name for the x axis (optional).}
  \item{ylab}{Character. The name for the y axis (optional).}
  \item{zlab}{Character. The name for the z axis (optional).}
}
\note{
The function creates in the working directory a new folder - namely "scatter2D3D" - that contains the HTML file, the PNG image as well as other necessary scripts.
}
\references{
\url{http://www.cranfield.ac.uk/health/researchareas/bioinformatics/page53292.html}
}
\author{
Eleni Chatzimichali <e.chatzimichali@cranfield.ac.uk>,
Conrad Bessant <c.bessant@cranfield.ac.uk>
}
\seealso{
\code{\link{iScatter2D}},
\code{\link{iScatter3D}}
}
\examples{

# Example 1
data(swiss)
iScatterPlots(swiss[,1], swiss[,2], swiss[,3], dataPoints=rownames(swiss),
  fileName="scatter2D3D", fileTitle="Interactive 2D/3D plot",
  fpng2d="scatter2D", fpng3d="scatter3D", plotTitle="2D/3D plot",
  xlab=colnames(swiss[1]), ylab=colnames(swiss[2]), zlab=colnames(swiss[3]))
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
