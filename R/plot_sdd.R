"plot_sdd" <-
function(datin=NULL, plotnew=TRUE, plothv=FALSE, plotweightedpts=FALSE, weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, points.col='black',
         points.pch=1, plotcentre=TRUE, centre.col='black', centre.pch=19, titletxt="Title", xaxis="Easting (m)", yaxis="Northing (m)", 
		 sdd.col='black', sdd.lwd=2, jpeg=FALSE, ...) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION DISTANCE (SDD) PLOT FUNCTION
  #  FUNCTION:  plot_sdd()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      24 AUGUST 2023
  #  CALLS:     jpeg()
  #  NOTES:     THE r.SDD OBJECT IS REQUIRED (GENERATED FROM THE 
  #             CALC_SDD FUNCTION) TO PLOT THE SDD CIRCLE. 
  #             THE PAR(...) OPTION ALLOWS FOR ADDITIONAL GRAPHICAL 
  #             PARAMETERS.
  #
  #=======================================================
  
  # SAVE PAR OPTIONS AND RETURN ON EXIT
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # CHECK FOR TYPE=SDE ON datin$TYPE TO TRAP ERROR RELATED TO INPUT DATA TYPE
  if(datin$TYPE == "SDD") {
  
    # ALLOW THE USER TO ACCESS AND MODIFY THE LIST OF GRAPHICAL PARAMETERS OF THE CURRENT PLOT.
    par(...)

    # EXTRACT APPROPRIATE LIST ELEMENT TO SIMPLIFY READING SCRIPT
    r.SDD <- datin$FORPLOTTING
  
    # DEFINE PLOT EXTENTS
    min.x <- min( (r.SDD$CENTRE.x - r.SDD$SDD), min(r.SDD$points[,1]) )
    max.x <- max( (r.SDD$CENTRE.x + r.SDD$SDD), max(r.SDD$points[,1]) )
    min.y <- min( (r.SDD$CENTRE.y - r.SDD$SDD), min(r.SDD$points[,2]) )
    max.y <- max( (r.SDD$CENTRE.y + r.SDD$SDD), max(r.SDD$points[,2]) )

    # JPEG OUTPUT OPTION
    if(jpeg) {
      jpeg(filename = paste("SDD",r.SDD$id,".jpg", sep=""), width = 600, height = 600, pointsize = 12, quality = 90, bg = "white", res = NA)
    } # END IF
		  
    # PLOT THE STANDARD DEVIATION DISTANCE CIRCLE
    if(plotnew) {
      plot(1, type="n", asp=1, xlab=xaxis, ylab=yaxis, xlim=c(min.x, max.x), ylim=c(min.y, max.y))
    } # END IF
		  
    lines(r.SDD$coordsSDD, col=sdd.col, lwd=sdd.lwd)
    title(paste(titletxt, sep=""))
		  
    if(plothv) {
      # DRAW HORIZONTAL AND VERTICAL LINES THROUGH CENTRE (NON-WEIGHTED/WEIGHTED/USER-DEFINED)
      abline(h=r.SDD$CENTRE.y, col=1, lty=2)
      abline(v=r.SDD$CENTRE.x, col=1, lty=2)
    } # END IF

    if(plotweightedpts) {
      # PLOT THE WEIGHTED POINTS
      points(r.SDD$points, cex=r.SDD$weights, col=weightedpts.col, pch=weightedpts.pch)
    } # END IF
		  
    if(plotpoints) {
      # PLOT THE POINTS
      points(r.SDD$points, col=points.col, pch=points.pch)
    } # END IF
		  
    if(plotcentre) {
      # ADD THE CENTRE POINT (NON-WEIGHTED/WEIGHTED/USER-DEFINED)
      points(r.SDD$CENTRE.x, r.SDD$CENTRE.y, col=centre.col, pch=centre.pch)
    } # END IF
		  
    if(jpeg) {
      # TURN OFF JPEG DEVICE WHEN DONE
      dev.off()
    } # END IF
         
  } # END IF - PASS ERROR TRAPPING
  
  else {
      warning("ERROR: Wrong calc function input for this function.\n")
      warning("Expected SDD; received ", datin$TYPE, "\n", sep="")
  } # END ELSE
         
} # END FUNCTION: plot_sdd
