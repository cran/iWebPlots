iSmoothPlot <- function(x, y, fileName, directory, ...) UseMethod("iSmoothPlot")


iSmoothPlot.default <- function(x, y=NULL, fileName, directory, fpng="smoothPlot", dataPoints=NULL, ...)
{	
	if((missing(directory)) || (is.null(directory)))
	{
		warning("You have to provide a valid path or directory.")
		
	} else if((missing(fileName)) || (is.null(fileName)))
	{
		warning("You have to provide a file name (without the extention \".html\").")
		
	} else
	{
		initialDir = getwd()
		dir.create(directory, recursive = TRUE, mode = "0777", showWarnings = FALSE)
		setwd(directory)
		fullDir = getwd()
		setwd(initialDir)
		
		x = as.matrix(x)
		
		if (missing(y)) 
		{
			if (ncol(x) == 1)
			{
				y = x
				x = 1:nrow(x)
				xlab = "Index"
				ylab = "x"
			}else
			{
				xlab = colnames(x)[1]
				ylab = colnames(x)[2]
				y = x[,2]
				x = x[,1]
			}
		}
			
		if(is.null(fpng))
		{
			fpng= "smoothPlot"
		}
		
		if(is.null(dataPoints))
		{
			x = as.matrix(x)

			if(is.null(rownames(x)))
			{
				dataPoints = seq(1:nrow(x))
			}else
			{
				dataPoints = rownames(x)
			}
		}
		
		y = as.matrix(y)
		
		fpng = paste(fpng,".png", sep="")
		png(paste(directory, fpng, sep="/"), width=680, height=470 , bg="transparent")
		smoothScatter(x, y, nrpoints=0, ...)
		points(x, y)
		cx2d = grconvertX(x , "user", "device")
		cy2d = grconvertY(y , "user", "device")
		dev.off()
			
		coords2d = cbind(cx2d, cy2d, cx2d+3, cy2d+3)
			
		id = seq(1:nrow(coords2d))
		
		con = openHtmlPage(paste(directory, fileName, sep="/"), title = "Interactive Smooth Plot")
		
		writeLines("<div id='main'>",con)
		writeLines("<div id='content'>",con)
		
		writeLines("<div class='imageMap' id='imageMap2d'>",con)
		imageMap(coords2d, con, list(ID=id, NAME=dataPoints, HREF=paste("#", dataPoints, sep="")), fpng)
		writeLines("</div>",con)
		
		writeLines("<form id='selection'></form>",con)
		
		writeLines("<div id='tooltip'>",con)
		writeLines("<div class='tipHeader'>Data point</div>",con)
		writeLines("<div class='tipBody'></div>",con)
		writeLines("<div class='tipFooter'></div>",con)
		writeLines("</div>",con)
		
		writeLines("</div>",con)
		writeLines("</div>",con)
		
		writeLines("<script src='js/jquery.js'></script>", con)
		writeLines("<script src='js/interactiveMaps.js'></script>", con)
		writeLines("<script src='js/imageMaps.js'></script>", con)
		closeHtmlPage(con)
		
		file.copy(system.file("js", package="iWebPlots"), directory, overwrite=TRUE, recursive=TRUE)
		file.copy(system.file("css", package="iWebPlots"), directory, overwrite=TRUE, recursive=TRUE)
		
		message(paste("The generated folder is located at ", fullDir, "",sep="\""))
	}
}


classify.iSmoothPlot <- function(x, y=NULL, classes, classColors=NULL, classNames=NULL, fileName, directory, fpng="smoothPlot", dataPoints=NULL, ...)
{
	if((missing(directory)) || (is.null(directory)))
	{
		warning("You have to provide a valid path or directory.")
		
	} else if((missing(fileName)) || (is.null(fileName)))
	{
		warning("You have to provide a file name (without the extention \".html\").")
		
	} else if((missing(classes)) || (is.null(classes)))
	{
		warning("You have to provide a vector containing the classes of x.")
		
	}else
	{
		initialDir = getwd()
		dir.create(directory, recursive = TRUE, mode = "0777", showWarnings = FALSE)
		setwd(directory)
		fullDir = getwd()
		setwd(initialDir)
		
		x = as.matrix(x)
		classes = as.factor(classes)
		classLevels = levels(classes)
		classNum = length(levels(classes))
		
		if (missing(y)) 
		{
			if (ncol(x) == 1)
			{
				y = x
				x = 1:nrow(x)
				xlab = "Index"
				ylab = "x"
			}else
			{
				xlab = colnames(x)[1]
				ylab = colnames(x)[2]
				y = x[,2]
				x = x[,1]
			}
		}
		
		if(is.null(fpng))
		{
			fpng= "smoothPlot"
		}
		
		if(is.null(classColors))
		{
			classColors = rainbow(classNum)
		}
		
		if(is.null(classNames))
		{
			for (i in 1:classNum)
			{
				classNames = c(classNames, paste("Class", i, sep=""))
			}
		}
			
		if(is.null(dataPoints))
		{
			x = as.matrix(x)
			
			if(is.null(rownames(x)))
			{
				dataPoints = seq(1:nrow(x))
			}else
			{
				dataPoints = rownames(x)
			}
		}
		
		y = as.matrix(y)
			
		fpng = paste(fpng,".png", sep="")
		png(paste(directory, fpng, sep="/"), width=680, height=470 , bg="transparent")
		smoothScatter(x, y, nrpoints=0, ...)
		
		for (i in 1:classNum)
		{
			temp = which(classes == classLevels[i])
			points(x[temp,], y[temp,], bg=classColors[i], pch=21)
		}
		
		legend("topright", title="Classes", legend=classNames, fill=classColors)
		cx2d = grconvertX(x , "user", "device")
		cy2d = grconvertY(y , "user", "device")
		dev.off()
			
		coords2d = cbind(cx2d, cy2d, cx2d+3, cy2d+3)
		
		id = seq(1:nrow(coords2d))
		
		con = openHtmlPage(paste(directory, fileName, sep="/"), title = "Interactive Smooth Plot")
		
		writeLines("<div id='main'>",con)
		writeLines("<div id='content'>",con)
		
		writeLines("<div class='imageMap' id='imageMap2d'>",con)
		imageMap(coords2d, con, list(ID=id, NAME=dataPoints, HREF=paste("#", dataPoints, sep="")), fpng)
		writeLines("</div>",con)
			
		writeLines("<form id='selection'></form>",con)
		
		writeLines("<div id='tooltip'>",con)
		writeLines("<div class='tipHeader'>Data point</div>",con)
		writeLines("<div class='tipBody'></div>",con)
		writeLines("<div class='tipFooter'></div>",con)
		writeLines("</div>",con)
		
		writeLines("</div>",con)
		writeLines("</div>",con)
		
		writeLines("<script src='js/jquery.js'></script>", con)
		writeLines("<script src='js/interactiveMaps.js'></script>", con)
		writeLines("<script src='js/imageMaps.js'></script>", con)
		closeHtmlPage(con)
		
		file.copy(system.file("js", package="iWebPlots"), directory, overwrite=TRUE, recursive=TRUE)
		file.copy(system.file("css", package="iWebPlots"), directory, overwrite=TRUE, recursive=TRUE)
		
		message(paste("The generated folder is located at ", fullDir, "",sep="\""))
	}
}

