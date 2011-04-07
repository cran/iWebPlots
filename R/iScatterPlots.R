iScatterPlots <- function(x, y, z= NULL, dataPoints, fileName, fileTitle, fpng2d, fpng3d, plotTitle= NULL, xlab= NULL, ylab= NULL, zlab= NULL)
{
	dir.create("scatter2D3D")
	
	fpng2d = paste(fpng2d,".png",sep="")
	png(paste("scatter2D3D", fpng2d,sep="/"), width=680, height=470 , bg="transparent")
	plot(x, y, panel.first = grid(7,7,col = "gray"), bg="blue", pch=21, xlab=xlab, ylab=ylab, main=plotTitle)
	cx2d = grconvertX(x , "user", "device")
	cy2d = grconvertY(y , "user", "device")
	dev.off()
	
	fpng3d = paste(fpng3d,".png",sep="")
	png(paste("scatter2D3D", fpng3d,sep="/"), bg="transparent",  width=680, height=470)
	s3d = scatterplot3d(x, y, z, grid=TRUE, main=plotTitle, xlab=xlab, ylab=ylab,  zlab=zlab, pch=21, bg="blue")
	A=s3d$xyz.convert(cbind(x,y,z))
	cx3d = grconvertX(A$x , "user", "device")
	cy3d = grconvertY(A$y , "user", "device")
	dev.off()
	
	
	coords2d = cbind(cx2d, cy2d, cx2d+3, cy2d+3)
	coords3d = cbind(cx3d, cy3d, cx3d+3, cy3d+3)
	
	id = seq(1:nrow(coords2d))
	
	con = openHtmlPage(paste("scatter2D3D",fileName, sep="/"), title=fileTitle)
		
	writeLines("<div id='main'>",con)
	writeLines("<div id='content'>",con)
	
	writeLines("<div id='banner'>",con)
	writeLines("<button type='button' onclick='alternate(0);' onmouseover='this.style.cursor=\"pointer\";'>Load 2D Plot</button>",con)
	writeLines("<button type='button' onclick='alternate(1);' onmouseover='this.style.cursor=\"pointer\";'>Load 3D Plot</button>",con)
	writeLines("</div>",con)
			   
	
	writeLines("<div class='imageMap' id='imageMap2d'>",con)
	imageMap(coords2d, con, list(ID=id, NAME=dataPoints, HREF=paste("#", dataPoints, sep="")), fpng2d)
	writeLines("</div>",con)
		
	writeLines("<div class='imageMap' id='imageMap3d' style='display: none'>",con)
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
	
	file.copy(system.file("js", "jquery.js", package="iWebPlots"), "scatter2D3D/jquery.js", overwrite=TRUE)
    file.copy(system.file("js", "imageMaps.js", package="iWebPlots"), "scatter2D3D/imageMaps.js", overwrite=TRUE)
    file.copy(system.file("js", "interactiveMaps.js", package="iWebPlots"), "scatter2D3D/interactiveMaps.js", overwrite=TRUE)
    file.copy(system.file("css", "style.css", package="iWebPlots"), "scatter2D3D/style.css", overwrite=TRUE)
  
}