iScatter3D <- function(x, y=NULL, z = NULL, dataPoints, fileName, fileTitle, fpng3d, plotTitle=NULL, xlab=NULL, ylab=NULL, zlab=NULL)
{
	dir.create("scatter3D")
	
	fpng3d = paste(fpng3d,".png",sep="")
	png(paste("scatter3D", fpng3d,sep="/"), bg="transparent",  width=680, height=470)
	s3d = scatterplot3d(x, y, z, grid=TRUE, main=plotTitle, xlab=xlab, ylab=ylab,  zlab=zlab, pch=21, bg="blue")
	A=s3d$xyz.convert(cbind(x,y,z))
	cx3d = grconvertX(A$x , "user", "device")
	cy3d = grconvertY(A$y , "user", "device")
	dev.off()
	
	
	coords3d = cbind(cx3d, cy3d, cx3d+3, cy3d+3)
	id = seq(1:nrow(coords3d))
	
	con = openHtmlPage(paste("scatter3D",fileName, sep="/"), title=fileTitle)
		
	writeLines("<div id='main'>",con)
	writeLines("<div id='content'>",con)
		
	writeLines("<div class='imageMap' id='imageMap3d'>",con)
	imageMap(coords3d, con, list(ID=id, NAME=dataPoints, HREF=paste("#", dataPoints, sep="")), fpng3d)
	writeLines("</div>",con)
		
	writeLines("<form id='selection'></form>",con)
	
	writeLines("<div id='tooltip'>",con)
	writeLines("<div class='tipHeader'>Data Point</div>",con)
	writeLines("<div class='tipBody'></div>",con)
	writeLines("<div class='tipFooter'></div>",con)
  	writeLines("</div>",con)
	
	writeLines("</div>",con)
	writeLines("</div>",con)
	
	writeLines("<script src='jquery.js'></script>", con)
	writeLines("<script src='interactiveMaps.js'></script>", con)
	writeLines("<script src='imageMaps.js'></script>", con)
	closeHtmlPage(con)
	
	file.copy(system.file("js", "jquery.js", package="iWebPlots"), "scatter3D/jquery.js", overwrite=TRUE)
	file.copy(system.file("js", "imageMaps.js", package="iWebPlots"), "scatter3D/imageMaps.js", overwrite=TRUE)
	file.copy(system.file("js", "interactiveMaps.js", package="iWebPlots"), "scatter3D/interactiveMaps.js", overwrite=TRUE)
	file.copy(system.file("css", "style.css", package="iWebPlots"), "scatter3D/style.css", overwrite=TRUE)
}