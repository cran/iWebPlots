iScatter2D <- function(x, y, dataPoints, fileName, fileTitle, fpng2d, plotTitle=NULL, xlab=NULL, ylab=NULL)
{
	dir.create("scatter2D")
	
	fpng2d = paste(fpng2d,".png", sep="")
	png(paste("scatter2D", fpng2d, sep="/"), width=680, height=470 , bg="transparent")
	plot(x, y, panel.first = grid(7,7,col = "gray"), bg="blue", pch=21, xlab=xlab, ylab=ylab, main=plotTitle)
	cx2d = grconvertX(x , "user", "device")
	cy2d = grconvertY(y , "user", "device")
	dev.off()
	
	coords2d = cbind(cx2d, cy2d, cx2d+3, cy2d+3)
	
	id = seq(1:nrow(coords2d))
	
	con = openHtmlPage(paste("scatter2D",fileName, sep="/"), title=fileTitle)
		
	writeLines("<div id='main'>",con)
	writeLines("<div id='content'>",con)
	
	writeLines("<div class='imageMap' id='imageMap2d'>",con)
	imageMap(coords2d, con, list(ID=id, NAME=dataPoints, HREF=paste("#", dataPoints, sep="")), fpng2d)
	writeLines("</div>",con)
		
	writeLines("<form id='selection'></form>",con)
	
	writeLines("<div id='tooltip'>",con)
	writeLines("<div class='tipHeader'>Data point</div>",con)
	writeLines("<div class='tipBody'></div>",con)
	writeLines("<div class='tipFooter'></div>",con)
  	writeLines("</div>",con)
	
	writeLines("</div>",con)
	writeLines("</div>",con)
	
	writeLines("<script src='jquery.js'></script>", con)
	writeLines("<script src='interactiveMaps.js'></script>", con)
	writeLines("<script src='imageMaps.js'></script>", con)
	closeHtmlPage(con)
	
	file.copy(system.file("js", "jquery.js", package="iWebPlots"), "scatter2D/jquery.js", overwrite=TRUE)
	file.copy(system.file("js", "imageMaps.js", package="iWebPlots"), "scatter2D/imageMaps.js", overwrite=TRUE)
	file.copy(system.file("js", "interactiveMaps.js", package="iWebPlots"), "scatter2D/interactiveMaps.js", overwrite=TRUE)
	file.copy(system.file("css", "style.css", package="iWebPlots"), "scatter2D/style.css", overwrite=TRUE)
}