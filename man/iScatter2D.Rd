\name{iScatter2D}
\alias{iScatter2D}
\title{
Interactive 2D Scatter Plot
}
\description{
Constructs an interactive two-dimensional (2D) graph (scatter plot) in an HTML wrapper.
}
\usage{
iScatter2D(x, y, dataPoints, fileName, fileTitle, fpng2d, plotTitle = NULL,
    xlab = NULL, ylab = NULL)
}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y.}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{fileTitle}{Character. The title for the generated HTML page.}
  \item{fpng2d}{Character. The name of the image file that contains the plot (without the extension of the image).}
  \item{plotTitle}{Character. The name of an overall title for the plot (optional).}
  \item{xlab}{Character. The name for the x axis (optional).}
  \item{ylab}{Character. The name for the y axis (optional).}
}
\note{
The function creates in the working directory a new folder - namely "scatter2D" - that contains the HTML file, the PNG image as well as other necessary scripts.
}
\references{
\url{http://www.cranfield.ac.uk/health/researchareas/bioinformatics/page53292.html}
}
\author{
Eleni Chatzimichali <e.chatzimichali@cranfield.ac.uk>,
Conrad Bessant <c.bessant@cranfield.ac.uk>
}
\seealso{
\code{\link{iScatter3D}},
\code{\link{iScatterPlots}}
}
\examples{

# Example 1
data(swiss)
iScatter2D(swiss[,1], swiss[,2], dataPoints=rownames(swiss),
  fileName="scatter2D", fileTitle="Interactive 2D plot",
  fpng2d="image2D", plotTitle="Two-dimensional plot",
  xlab=colnames(swiss[1]), ylab=colnames(swiss[2]))
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
