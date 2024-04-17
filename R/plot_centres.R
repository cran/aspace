"plot_centres" <-
function(datin=NULL, plotnew=FALSE, plotSDE=FALSE, xaxis="Easting (m)", yaxis="Northing (m)", plotweightedpts=FALSE, weightedpts.col='black', weightedpts.pch=19, plotpoints=TRUE, points.col='black', points.pch=1, plotcentre=FALSE, centre.col='black', centre.pch=19, plotcentral=FALSE, central.col='green', central.pch=19, plotCF2PTS=FALSE, CF2PTS.col='orange', CF2PTS.pch=19, plotmedian=FALSE, median.col='blue', median.pch=17, plotCMD=FALSE, CMD.col='red', CMD.pch=17, TITLE="Title", ...) {

  #=======================================================
  #
  #  TITLE:     CENTRES PLOT FUNCTION
  #  FUNCTION:  plot_centres()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      17 APRIL 2024
  #  NOTES:     THE datin ARGUMENT IS LIST OF OUTPUT OBJECTS FROM calc_ FUNCTIONS
  #             THE r.SDD/r.SDE/r.BOX OBJECT IS REQUIRED (GENERATED FROM THE
  #             CALC_SDD/CALC_SDE/CALC_BOX FUNCTION) TO PLOT. 
  #             THE PAR(...) OPTION ALLOWS FOR ADDITIONAL GRAPHICAL 
  #             PARAMETERS.
  #
  # CALL:       # MNC (BLACK CIRCLE)
  #             a <- calc_mnc(points=activities)
  #
  #             # MDC (BLUE TRIANGLE)
  #             b <- calc_mdc(points=activities)
  #
  #             # CF (GREEN CIRCLE)
  #             d <- calc_cf(points=activities)
  #
  #             # CMD (RED TRIANGLE)
  #             e <- calc_cmd(points=activities)
  #
  #             # CF2PTS (ORANGE CIRCLE)
  #             f <- calc_cf2pts(points1=activities, points2=activities2)
  #
  #             # BUILD LIST OF OBJECTS TO PASS AS THE datin ARGUMENT
  #             robjects <- list(a,b,d,e,f)
  #             # CALL THE PLOT FUNCTION
  #             plot_centres(datin=robjects, plotnew=TRUE, plotcentre=TRUE, plotmedian=FALSE, plotcentral=TRUE, plotCMD=TRUE, plotCF2PTS=TRUE)
  #
  #=======================================================
  
  # SAVE PAR OPTIONS AND RETURN ON EXIT
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
 
  # ALLOW THE USER TO ACCESS AND MODIFY THE LIST OF GRAPHICAL PARAMETERS OF THE CURRENT PLOT.
  par(...)
	 
  # COUNT THE NUMBER OF OBJECTS IN THE INPUT LIST; EACH IS A SPECIFIC PLOT
  nobj <- length(datin)
  
  # ENSURE THAT THERE ARE ENTRIES IN THE ARGUMENT; IF NOT, PROVIDE WARNING AND EXIT
  if(nobj >= 1) {
    # THERE ARE OBJECTS PASSED IN THE LIST FOR PROCESSING
  
    # CONVERT THE INPUT LIST OBJECT ELEMENTS INTO A SUMMARY TABLE TO GUIDE PROCESSING
    # BEGIN BY READING THE $TYPE ELEMENTS IN EACH LIST OBJECT INTO A VECTOR
    out <- NULL
    for(listitem in 1:nobj) {
      out <- c(out, datin[[listitem]]$TYPE)
    } # END FOR: listitem
    # CONVERT TO DATA FRAME AND ADD COLUMN NAMES
    out <- as.data.frame(cbind(1:nobj, out))
    names(out) <- c("ListPosition", "TYPE")

    # EACH ROW OF THE DATAFRAME out IS A SPECIFIC PLOT TO BE ADDED
  
    # PLOT THE STANDARD DEVIATION DISTANCE CIRCLE
    if(plotnew) {
      
      # DEFINE THE PLOT EXTENTS
      if("CF2PTS" %in% out[,2]) {
        # SET EXTENTS TO THE RANGE OF TWO INPUT SETS OF POINTS
        objpointer <- which(out[,2] == "CF2PTS")
        robject <- datin[[objpointer]]$FORPLOTTING
        min.x <- min(robject$points1[,1], robject$points2[,1])
        max.x <- max(robject$points1[,1], robject$points2[,1])
        min.y <- min(robject$points1[,2], robject$points2[,2])
        max.y <- min(robject$points1[,2], robject$points2[,2])
      } # END IF
      else {
        # SET EXTENTS TO THE RANGE OF THE SINGLE IMPUT SET OF POINTS
        robject <- datin[[1]]$FORPLOTTING
        min.x <- min(robject$points[,1])
        max.x <- max(robject$points[,1])
        min.y <- min(robject$points[,2])
        max.y <- min(robject$points[,2])
      } # END ELSE
      
      # PRODUCE BASE PLOT
      plot(1, type="n", asp=1, xlab=xaxis, ylab=yaxis, xlim=c(min.x, max.x), ylim=c(min.y, max.y))
    
    } # END IF
		  
    if(plotweightedpts) {
      # PLOT THE WEIGHTED POINTS
      points(datin[[1]]$FORPLOTTING$points, cex=robject$weights, col=weightedpts.col, pch=weightedpts.pch)
    } # END IF
		  
    if(plotpoints) {
      # PLOT THE POINTS
      points(datin[[1]]$FORPLOTTING$points, col=points.col, pch=points.pch)
    } # END IF
		  
    # PLOT THE VARIOUS REQUESTED CENTROIDS (EACH NEEDS ITS APPROPRIATE OBJECT PRODUCED BY
    # THE CORRESPONDING calc_ FUNCTION TO BE INCLUDED IN THE datin LIST ARGUMENT)
  
    # THERE ARE 5 POSSIBLE CENTROIDS TO PLOT: CMD, CF, CF2PTS, MNC, MDC
    # FOR EACH OF nrow(out) CALL THE APPROPRIATE PLOTTING FUNCTION
    for(plt in 1:nrow(out)) {
      if(out[plt,2] == "MNC" & plotcentre) {
        robject <- datin[[plt]]$FORPLOTTING
        points(robject$CENTRE.x, robject$CENTRE.y, col=centre.col, pch=centre.pch)
      }
      if(out[plt,2] == "MDC" & plotmedian) {
        robject <- datin[[plt]]$FORPLOTTING
        points(robject$median.x, robject$median.y, col=median.col, pch=median.pch)
      }
      if(out[plt,2] == "CMD" & plotCMD) {
        robject <- datin[[plt]]$FORPLOTTING
        points(robject$CMD.x, robject$CMD.y, col=CMD.col, pch=CMD.pch)
      }
      if(out[plt,2] == "CF" & plotcentral) {
        robject <- datin[[plt]]$FORPLOTTING
        points(robject$CF.x, robject$CF.y, col=central.col, pch=central.pch)
      }
      if(out[plt,2] == "CF2PTS" & plotCF2PTS) {
        if(plotpoints) {
          # PLOT THE POINTS
          robject <- datin[[plt]]$FORPLOTTING
          points(robject$points1, col=points.col, pch=points.pch)
          points(robject$points2, col=points.col, pch=points.pch)
        } # END IF
        # IDENTIFY THE CENTRAL FEATURE
        robject <- datin[[plt]]$FORPLOTTING
        points(robject$CF2PTS.x, robject$CF2PTS.y, col=CF2PTS.col, pch=CF2PTS.pch)
      }
    } # END FOR: plt
    
    # ADD TITLE TO THE PLOT
    title(TITLE)
    
  } # END IF
  else {
    warning("No calc objects provided as a list for the datin argument.")
    return()
  } # END ELSE
  
} # END FUNCTION: plot_centres
