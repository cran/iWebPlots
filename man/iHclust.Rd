\name{iHclust}
\alias{iHclust}
\alias{iHclust.default}
\alias{classify.iHclust}
\title{Interactive Hierarchical Clustering}
\description{Constructs an interactive hierarchical dendrogram in an HTML wrapper. Classification of the input dataset is also available. }
\usage{
iHclust(x, fileName, directory, ...)

\method{iHclust}{default}(x, fileName, directory, fpng = "dendrogram", labels = NULL,
          hang = -1, dataPoints = NULL, ...)

\method{classify}{iHclust}(x, classes, classColors=NULL, classNames=NULL, fileName, directory,
          fpng="dendrogram", dataPoints=NULL, ...)

}
\arguments{
  \item{x}{An object as produced by \code{\link{hclust}}.}
  \item{fileName}{Character. The name of the HTML file that contains the plot and image map (without the extension '.html').}
  \item{directory}{Character. The name of the directory or path to save the interactive plots and asssociated files.}
  \item{fpng}{Character. The name of the image file that contains the plot, without the extension of the image (optional).}
  \item{labels}{Character vector. The vector contains the names of the leaves of the dedrogram. By default the row names or row numbers of the original data are used.
  If labels=FALSE no labels at all are plotted.(optional)}
  \item{hang}{The fraction of the plot height by which labels should hang below the rest of the plot. A negative value will cause the labels to hang down from 0. (optional)}
  \item{dataPoints}{Character or numeric vector that holds the associated metadata for each point in the plot. It should have the same length as x and y. (optional)}
  \item{classes}{A vector or factor that contains the classes to be used.}
  \item{classColors}{Vector that contains the color for each class. It should have the same length as as classes. (optional)}
  \item{classNames}{Vector that contains the name for each class. It should have the same length as classes. (optional)}
  \item{\dots}{More parameters can be given as arguments (See \code{\link{hclust}} and \code{\link{par}}).}
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
  \code{\link{hclust}},
  \code{\link{dendrogram}},
  \code{\link{iScatterPlots}},
  \code{\link{iSmoothPlot}},
}
\examples{
## Example 1
hc <- hclust(dist(USArrests), "ave")
iHclust(hc, fileName="iHclust1", directory="iHclust1", fpng="iHclust1")

## Example 2 (Classification)
data(mtcars)
hc <- hclust(dist(mtcars[,1:7]), "ave")

classify.iHclust(hc, classes=mtcars[,10],  fileName="iHclust2", directory="iHclust2",
        fpng="iHclust2", main="Cluster Dendrogram with Classification")
        
}
\keyword{ iplot }
\keyword{ dynamic }
\keyword{ graph }
\keyword{ interface }
