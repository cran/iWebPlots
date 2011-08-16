\name{iSmoothPlot}
\alias{iSmoothPlot}
\alias{iSmoothPlot.default}
\alias{classify.iSmoothPlot}
\title{Interactive smoothed color density scatterplot}
\description{Constructs an interactive and smoothed color density representation of a scatterplot, obtained through a kernel density estimate.
The functions for kernel smoothing are loaded from the KernSmooth package by  M. P. Wand.
Classification of the input dataset is also available. 
}
\usage{
iSmoothPlot(x, y, fileName, directory, ...)

\method{iSmoothPlot}{default}(x, y = NULL, fileName, directory, fpng = "smoothPlot",
        dataPoints = NULL, ...)

\method{classify}{iSmoothPlot}(x, y = NULL, classes, classColors = NULL, classNames = NULL,
        fileName, directory, fpng = "smoothPlot", dataPoints = NULL, ...)

}
\arguments{
  \item{x}{the x coordinates of points in the plot}
  \item{y}{the y coordinates of points in the plot (optional)}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng}{Character. The name of the image file that contains the plot, without the extension of the image. (optional)}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y. (optional)}
  \item{classes}{A vector or factor that contains the distinct classes.}
  \item{classColors}{Vector that contains the color for each class. It should have the same length as classes. (optional)}
  \item{classNames}{Vector that contains the name for each class. It should have the same length as classes. (optional)}
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
  \code{\link{smoothScatter}},
  \code{\link{iScatter2D}},
  \code{\link{iScatter3D}},
  \code{\link{iScatterPlots}}
}
\examples{

# Example 1
n <- 500
x1  <- matrix(rnorm(n), ncol=2)
x2  <- matrix(rnorm(n, mean=3, sd=1.5), ncol=2)
x   <- rbind(x1,x2)

iSmoothPlot(x, dataPoints=rownames(x),
  fileName="smoothPlot1", directory="smoothPlot1", 
  fpng="smoothPlot1", main="Smooth Scatter Plot")
  
  
# Example 2 - Classification Example

data(iris)

classify.iSmoothPlot(iris[,1:2], classes=iris[,5], classColors=c("red","orange","green"), 
classNames=c("setosa","versicolor","virginica"), fileName="iSmoothIris", 
directory="iSmoothIris", fpng="iSmoothIris", xlab="Sepal.Lenght", ylab="Sepal.Width",
main="Edgar Anderson's Iris Data\n(Interactive Scatterplot with Smoothed Densities)")

}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
