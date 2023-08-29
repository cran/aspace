"plot_sde" <-
function(datin=NULL, plotnew=TRUE, plotSDEaxes=FALSE, plotweightedpts=FALSE, weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, points.col='black',
         points.pch=1, plotcentre=TRUE, centre.col='black', centre.pch=19, titletxt="Title", xaxis="Easting (m)", yaxis="Northing (m)", 
		 sde.col='black', sde.lwd=2, jpeg=FALSE, ...) {
		 
  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION ELLIPSE (SDE) PLOT FUNCTION
  #  FUNCTION:  plot_sde()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      24 AUGUST 2023
  #  CALLS:     jpeg()
  #  NOTES:     THE r.SDE OBJECT IS REQUIRED (GENERATED FROM THE CALC_SDE FUNCTION)
  #             TO PLOT THE SDE. THE PAR(...) OPTION ALLOWS 
  #             FOR ADDITIONAL GRAPHICAL PARAMETERS.
  #
  #=======================================================

  # SAVE PAR OPTIONS AND RETURN ON EXIT
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # CHECK FOR TYPE=SDE ON datin$TYPE TO TRAP ERROR RELATED TO INPUT DATA TYPE
  if(datin$TYPE == "SDE") {
  
    # ALLOW THE USER TO ACCESS AND MODIFY THE LIST OF GRAPHICAL PARAMETERS OF THE CURRENT PLOT.
    par(...)

    # EXTRACT APPROPRIATE LIST ELEMENT TO SIMPLIFY READING SCRIPT
    r.SDE <- datin$FORPLOTTING
  
    # DEFINE PLOT EXTENTS
    min.x <- min( (r.SDE$CENTRE.x - max(r.SDE$Sigma.x,r.SDE$Sigma.y)), min(r.SDE$points[,1]) )
    max.x <- max( (r.SDE$CENTRE.x + max(r.SDE$Sigma.x,r.SDE$Sigma.y)), max(r.SDE$points[,1]) )
    min.y <- min( (r.SDE$CENTRE.y - max(r.SDE$Sigma.x,r.SDE$Sigma.y)), min(r.SDE$points[,2]) )
    max.y <- max( (r.SDE$CENTRE.y + max(r.SDE$Sigma.x,r.SDE$Sigma.y)), max(r.SDE$points[,2]) )

    # JPEG OUTPUT OPTION
    if(jpeg) {
      jpeg(filename = paste("SDE",r.SDE$id,".jpg", sep=""), width = 600, height = 600, pointsize = 12, quality = 90, bg = "white", res = NA)
    } # END IF
	  
    # PLOT THE STANDARD DEVIATION ELLIPSE
    if(plotnew) {
      plot(1, type="n", asp=1, xlab=xaxis, ylab=yaxis, xlim=c(min.x, max.x), ylim=c(min.y, max.y))
    } # END IF
	  
    lines(r.SDE$coordsSDE, col=sde.col, lwd=sde.lwd)
    title(paste(titletxt, sep=""))
	  
    if(plotSDEaxes) {
      # PLOT HALF-AXES (SIGMA-Y) FOR ELLIPSE IN GREEN
      xprime <- r.SDE$CENTRE.x + ( r.SDE$Sigma.y * cos_d(90-r.SDE$theta) )
      yprime <- r.SDE$CENTRE.y + ( r.SDE$Sigma.y * sin_d(90-r.SDE$theta) )
      segments(r.SDE$CENTRE.x, r.SDE$CENTRE.y, xprime, yprime, col=3)
	    
      # PLOT HALF-AXES (SIGMA-X) FOR ELLIPSE IN BLUE
      xprime <- r.SDE$CENTRE.x + ( r.SDE$Sigma.x * cos_d(r.SDE$theta) )
      yprime <- r.SDE$CENTRE.y - ( r.SDE$Sigma.x * sin_d(r.SDE$theta) )
      segments(r.SDE$CENTRE.x, r.SDE$CENTRE.y, xprime, yprime, col=4)
    } # END IF
	  
    if(plotweightedpts) {
      # PLOT THE WEIGHTED POINTS
      points(r.SDE$points, cex=r.SDE$weights, col=weightedpts.col, pch=weightedpts.pch)
    } # END IF
	  
    if(plotpoints) {
      # PLOT THE POINTS
      points(r.SDE$points, col=points.col, pch=points.pch)
    } # END IF
		  
    if(plotcentre) {
      # ADD THE CENTRE POINT (NON-WEIGHTED/WEIGHTED/USER-DEFINED)
      points(r.SDE$CENTRE.x, r.SDE$CENTRE.y, col=centre.col, pch=centre.pch)
    } # END IF

    if(jpeg) {
      # TURN OFF JPEG DEVICE WHEN DONE
      dev.off()
    } # END IF
  
  } # END IF - PASS ERROR TRAPPING
  
  else {
      warning("ERROR: Wrong calc function input for this function.\n")
      warning("Expected SDE; received ", datin$TYPE, "\n", sep="")
  } # END ELSE
  
} # END FUNCTION: plot_sde
