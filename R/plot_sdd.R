"plot_sdd" <-
function(plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE, weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, points.col='black', 
         points.pch=1, plotcentre=TRUE, centre.col='black', centre.pch=19, plotcentral=FALSE, central.col='green', central.pch=19, plotmedian=FALSE, 
		 median.col='blue', median.pch=17, plotCMD=FALSE, CMD.col='red', CMD.pch=17, titletxt="Title", xaxis="Easting (m)", yaxis="Northing (m)", 
		 sdd.col='black', sdd.lwd=2, jpeg=FALSE, ...) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION DISTANCE (SDD) PLOT FUNCTION
  #  FUNCTION:  plot_sdd()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      November 21, 2009
  #  CALLS:     ellipse3(), jpeg()
  #  NOTES:     THE r.SDD OBJECT IS REQUIRED (GENERATED FROM THE 
  #             CALC_SDD FUNCTION) TO PLOT THE SDD CIRCLE. 
  #             THE PAR(...) OPTION ALLOWS FOR ADDITIONAL GRAPHICAL 
  #             PARAMETERS.
  #
  #=======================================================

	# ALLOW THE USER TO ACCESS AND MODIFY THE LIST OF GRAPHICAL PARAMETERS OF THE CURRENT PLOT.
	par(...)
	
	      # DEFINE PLOT EXTENTS
	      min.x <- min( (r.SDD$CENTRE.x - r.SDD$SDD), min(r.SDD$points[,1]) )
	      max.x <- max( (r.SDD$CENTRE.x + r.SDD$SDD), max(r.SDD$points[,1]) )
	      min.y <- min( (r.SDD$CENTRE.y - r.SDD$SDD), min(r.SDD$points[,2]) )
	      max.y <- max( (r.SDD$CENTRE.y + r.SDD$SDD), max(r.SDD$points[,2]) )

		  # JPEG OUTPUT OPTION
		  if(jpeg) {
			jpeg(filename = paste("SDD",r.SDD$id,".jpg", sep=""), width = 600, height = 600, pointsize = 12, quality = 90, bg = "white", res = NA)
		  }
		  
		  # PLOT THE STANDARD DEVIATION DISTANCE CIRCLE
		  if(plotnew) {
	      plot(1, type="n", asp=1, xlab=xaxis, ylab=yaxis, xlim=c(min.x, max.x), ylim=c(min.y, max.y))
		  }
		  
	      ellipse3(r.SDD$CENTRE.x, r.SDD$CENTRE.y, r.SDD$SDD, r.SDD$SDD, as_radians(0), col=sdd.col, lwd=sdd.lwd)
		  title(paste(titletxt, sep=""))
		  
	      if(plothv) {
	        # DRAW HORIZONTAL AND VERTICAL LINES THROUGH CENTRE (NON-WEIGHTED/WEIGHTED/USER-DEFINED)
	        abline(h=r.SDD$CENTRE.y, col=1, lty = 2)
	        abline(v=r.SDD$CENTRE.x, col=1, lty = 2)
	      }

	      if(plotweightedpts) {
	        # PLOT THE WEIGHTED POINTS	
	        points(r.SDD$points, cex=r.SDD$weights, col=weightedpts.col, pch=weightedpts.pch)
	      }
		  
	      if(plotpoints) {
	        # PLOT THE POINTS	
	        points(r.SDD$points, col=points.col, pch=points.pch)
	      }
		  
	      if(plotcentre) {
	        # ADD THE CENTRE POINT (NON-WEIGHTED/WEIGHTED/USER-DEFINED)
	        points(r.SDD$CENTRE.x, r.SDD$CENTRE.y, col=centre.col, pch=centre.pch)
	      }
		  
		  if(plotcentral) {
	        # IDENTIFY THE CENTRAL FEATURE
	        points(r.SDD$central.x, r.SDD$central.y, col=central.col, pch=central.pch)
	      }

		  if(plotmedian) {
	        # ADD MEDIAN CENTRE POINT
	        points(r.SDD$median.x, r.SDD$median.y, col=median.col, pch=median.pch)
	      }
		  
		  if(plotCMD) {
	        # ADD THE CENTRE OF MINIMUM DISTANCE POINT
	        points(r.SDD$CMD.x, r.SDD$CMD.y, col=CMD.col, pch=CMD.pch)
	      }
		  
		  if(jpeg) {
			# TURN OFF JPEG DEVICE WHEN DONE
			dev.off()
		  }
	}