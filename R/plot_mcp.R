"plot_mcp" <-
function(plotnew=TRUE, plotpoints=TRUE, points.col='black', points.pch=1, titletxt="Title", xaxis= "Easting (m)", yaxis="Northing (m)", mcp.col='black', mcp.lwd=2, fill.col=NA, jpeg=FALSE, ...) {

  #=======================================================
  #
  #  TITLE:     MINIMUM CONVEX POLYGON (MCP) PLOT FUNCTION
  #  FUNCTION:  plot_mcp()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO REMMEL
  #  DATE:      November 21, 2009
  #  NEEDS:     LIBRARY: adehabitat
  #  NOTES:     THE r.MCP OBJECT IS REQUIRED (GENERATED FROM THE CALC_MCP FUNCTION) 
  #             TO PLOT THE MCP. THE PAR(...) OPTION ALLOWS FOR ADDITIONAL GRAPHICAL PARAMETERS.
  #
  #=======================================================
  
	# ALLOW THE USER TO ACCESS AND MODIFY THE LIST OF GRAPHICAL PARAMETERS OF THE CURRENT PLOT.
	par(...)
	
	# JPEG OUTPUT OPTION
	if(jpeg) {
		jpeg(filename = paste("MCP",r.MCP$id,".jpg", sep=""), width = 600, height = 600, pointsize = 12, quality = 90, bg = "white", res = NA)
	  }	
	
    if(plotnew) {
	# PLOT THE MINIMUM CONVEX POLYGON (NEW PLOT)
	plot(r.MCP$MCP, xlab=xaxis, ylab=yaxis, colpol=fill.col, col=mcp.col, lwd=mcp.lwd)
	title(paste(titletxt, sep=""))
	  }	
    else {	
	# PLOT THE MINIMUM CONVEX POLYGON (OVERLAY PLOT)
	default.parameters <- par(no.readonly = TRUE)
	xlim.max <- c(default.parameters$usr[1],default.parameters$usr[2])
	ylim.max <- c(default.parameters$usr[3],default.parameters$usr[4])
	plot(r.MCP$MCP, xlab=xaxis, ylab=yaxis, colpol=fill.col, col=mcp.col, lwd=mcp.lwd, xlim=xlim.max, ylim=ylim.max, add=TRUE)
	title(paste(titletxt, sep=""))
	  }
	
	if(plotpoints) {
	# PLOT THE POINTS
		points(r.MCP$points, col=points.col, pch=points.pch)
	  }  

	if(jpeg) {
	# TURN OFF JPEG DEVICE WHEN DONE
		dev.off()
	  } 
  }

