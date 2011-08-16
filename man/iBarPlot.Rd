\name{iBarPlot}
\alias{iBarPlot}
\alias{iBarPlot.default}
\title{Interactive Barplots}
\description{Constructs an interactive barplot in an HTML wrapper.}
\usage{
iBarPlot(height, fileName, directory, ...)

\method{iBarPlot}{default}(height, fileName, directory, fpng = "barplot",
        dataPoints = NULL, width = 1, space = 0.2, horiz = FALSE,
        names.arg = NULL, beside = FALSE, axis = FALSE, ...)

}
\arguments{
  \item{height}{Vector or matrix. The input data describe the bars that make up the plot. For more information see \code{\link{barplot}}}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng}{Character. The name of the image file that contains the plot, without the extension of the image. (optional)}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y. (optional)}
  \item{width}{Vector that contains the bar widths. It is re-cycled to length the number of bars.(optional)}
  \item{space}{The amount of space (as a fraction of the average bar width) left before each bar. For more information see \code{\link{barplot}}. (optional)}
  \item{horiz}{Logical. If FALSE, the bars are drawn vertically, whereas if TRUE, the bars are drawn horizontally. (optional)}
  \item{names.arg}{A vector that provides the names to be plotted below each bar or group of bars. (optional)}
  \item{beside}{Logical. If FALSE, the columns of height are portrayed as stacked bars, and if TRUE the columns are portrayed as juxtaposed bars. (optional)}
  \item{axis}{Logical. The attribute specifies whether an axis should be drawn. (optional)}
  \item{\dots}{More parameters can be given as arguments (See \code{\link{barplot}} and \code{\link{par}}).}
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
  \code{\link{iHist}},
  \code{\link{barplot}},
  \code{\link{hist}}
}
\examples{

# Example 1
cars <- c(1, 3, 6, 4, 9)
iBarPlot(cars, fileName="barplot1", directory="barplot1", fpng="barplot1",
        main="Interactive Barplot 1")
        
# Example 2
iBarPlot(VADeaths, beside = TRUE, col=rainbow(20), fileName="barplot2",
        directory="barplot2", fpng="barplot2",
        main="Death Rates in Virginia (1940)\n(Interactive Barplot)",
        dataPoints=c(rownames(as.matrix(VADeaths[,1])),
            rownames(as.matrix(VADeaths[,2])),
            rownames(as.matrix(VADeaths[,3])),
            rownames(as.matrix(VADeaths[,4]))))
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
