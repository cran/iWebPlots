\name{iScatter3D}
\alias{iScatter3D}
\title{
Interactive 3D Scatter Plot
}
\description{
Constructs an interactive three-dimensional (3D) graph (scatter plot) in an HTML wrapper
}
\usage{
iScatter3D(x, y = NULL, z = NULL, dataPoints, fileName, fileTitle, fpng3d,
    plotTitle = NULL, xlab = NULL, ylab = NULL, zlab = NULL)
}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot}
  \item{z}{the z coordinates of points in the plot}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y.}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{fileTitle}{Character. The title for the generated HTML page.}
  \item{fpng3d}{Character. The name of the image file that contains the plot (without the extension of the image).}
  \item{plotTitle}{Character. Name of an overall title for the plot (optional).}
  \item{xlab}{Character. The name for the x axis (optional).}
  \item{ylab}{Character. The name for the y axis (optional).}
  \item{zlab}{Character. The name for the z axis (optional).}
}
\note{
The function creates in the working directory a new folder - namely "scatter3D" - that contains the HTML file, the PNG image as well as other necessary scripts.
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
\code{\link{iScatterPlots}}
}
\examples{

# Example 1
data(swiss)
iScatter3D(swiss[,1], swiss[,2], swiss[,3], dataPoints=rownames(swiss),
  fileName="scatter3D", fileTitle="Interactive 3D plot",
  fpng3d="image3D", plotTitle="Three-dimensional plot",
  xlab=colnames(swiss[1]), ylab=colnames(swiss[2]), zlab=colnames(swiss[3]))
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
