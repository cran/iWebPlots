\name{iHist}
\alias{iHist}
\alias{iHist.default}
\title{Interactive Histograms}
\description{Constructs an interactive histogram in an HTML wrapper.}
\usage{
iHist(x, breaks, fileName, directory, ...)

\method{iHist}{default}(x, breaks, fileName, directory, fpng = "histogram",
        dataPoints = NULL, freq = NULL, prob = !freq, ...)

}
\arguments{
  \item{x}{a vector of values used to construct the histogram.}
  \item{breaks}{a vector that provides the breakpoints between the cells of a histogram. (optional)}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng}{Character. The name of the image file that contains the plot, without the extension of the image (optional).}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y. (optional)}
  \item{freq}{Logical. If TRUE, the histogram represents frequencies. Otherwise, if FALSE, probability densities are plotted. It defaults to TRUE if (and only if) the breaks are equidistant (and probability is not specified). (optional)}
  \item{prob}{an alias for !freq}
  \item{\dots}{More parameters can be given as arguments (See \code{\link{hist}} and \code{\link{par}}).}
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
  \code{\link{hist}},
  \code{\link{iBarPlot}}
}
\examples{

# Example 1 - Equidistant breaks
iHist(islands, fileName="hist1", directory="hist1", fpng="hist1",
      main="Interactive Histogram 1")

# Example 2 - Equidistant breaks
iHist(sqrt(islands), breaks = 12, col="lightblue", border="pink",
    fileName="hist2", directory="hist2", fpng="hist2",
    main="Interactive Histogram 2")

# Example 3 - Distances between breaks that are NOT equal
iHist(sqrt(islands), breaks = c(4*0:5, 10*3:5, 70, 100, 140), col="blue1",
    fileName="hist3", directory="hist3", fpng="hist3",
    main="Interactive Histogram 3")

}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
