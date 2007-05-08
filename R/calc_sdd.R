"calc_sdd" <-
function(id=1, filename="SDD_Output.txt", centre.xy=centre, calccentre=TRUE, useWMC=FALSE, weightpoints=FALSE, weights=wts, destmat=activities, verbose=FALSE, plot=TRUE, plothv=TRUE, plotdest=TRUE, plotcenter=TRUE, box=TRUE) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION DISTANCE (SDD) CALCULATOR
  #  FUNCTION:  calc_sdd()
  #  AUTHOR:    TARMO K. REMMEL, RON BULIUNG 
  #  DATE:      27 MARCH 2006
  #  CALLS:     distances(), ellipse(), as_radians()
  #  NEEDS:     
  #  NOTES:     SEVERAL OUTPUT VARIABLES ARE INITIALIZED TO -999,
  #             THUS, IF THESE VALUES ARE SEEN IN THE OUTPUT, THEIR
  #             VALUES WERE NEVER SET WITHIN THE FUNCTION.
  #             USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE SDD CIRCLE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE SDD CIRCLES ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE IT TO CREATE SHAPEFILES AFTERWARDS.
  #
  #  ERROR:     1000  NO ERRORS DETECTED
  #               25  TOO FEW ACTIVITIES, NEED >= 3
  #
  #  OUTPUT:	ID		UNIQUE SDD IDENTIFIER
  #		calccentre	T|F SHOULD MEAN CENTER BE COMPUTED
  #		Orig.x		ORIGINAL X-COORDINATE BEFORE MODIFICATION - CURRENTLY NOT USED
  #		Orig.y		ORIGINAL Y-COORDINATE BEFORE MODIFICATION - CURRENTLY NOT USED
  #		CENTRE.x	X-COORDINATE USED FOR CENTRE
  #		CENTRE.y	Y-COORDINATE USED FOR CENTRE
  #		SD.x		ORTHOGONAL STD. DEV IN X-DIRECTION
  #		SD.y		ORTHOGONAL STD. DEV IN Y-DIRECTION
  #		SDD.radius	RADIUS OF SDD
  #		Box.area	AREA OF ORTHOGONAL STD. DEV BOX
  #		SDD.area	AREA OF SDD
  #		useWMC		T|F SHOULD WEIGHTED MEAN CENTER BE USED
  #		WeightPoints	T|F SHOULD DESTINATION POINTS BE WEIGHTED
  #
  #=======================================================
  
  # GIVEN THE VECTOR OF DISTANCES TO A HOUSEHOLD di COMPUTE SDD

  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000
    
  # INITIALIZE VARIABLES
  areabox <- -999
  SDx <- -999
  SDy <- -999
  Orig.x <- -999
  Orig.y <- -999
  
  # STORE THE COUNT OF DESTINATION POINTS
  n <- dim(destmat)[1]

  if(calccentre) {
    if(useWMC) {
      # COMPUTE AND USE WEIGHTED MEAN CENTER RATHER THAN HOUSEHOLD LOCATION AS CENTER (WEIGHTED)
      weighted.x <- destmat[,1] * weights
      weighted.y <- destmat[,2] * weights
      WMC.x <- c( sum(weighted.x) / sum(weights) )
      WMC.y <- c( sum(weighted.y) / sum(weights) )    
      centre.xy[1] <- WMC.x
      centre.xy[2] <- WMC.y
    }
    else {
      # COMPUTE AND USE MEAN CENTER RATHER THAN HOUSEHOLD LOCATION AS CENTER (NON-WEIGHTED)
      meanx <- sum(destmat[,1])/n
      meany <- sum(destmat[,2])/n
      centre.xy[1] <- meanx
      centre.xy[2] <- meany
    }
  }
  
  
  # WEIGHT DESTINATION POINTS HERE
  destmat[,1] * weights
  destmat[,2] * weights
    
  
  # INITIALIZE FUNCTION VARIABLE WITH PARAMETER VALUE
  dist.v <- distances(centre.xy, destmat)
  
  # TEST WHETHER A SUFFICIENT NUMBER OF ACTIVITIES WERE SUPPLIED
  if(length(dist.v) >= 3) {
    
    # PERFORM THE STANDARD DEVIATION DISTANCE COMPUTATION (SDD)
    SDD <- sqrt( sum(dist.v^2/(length(dist.v) - 2) ) )

    # COMPUTE SDD AREA
    sddarea <- pi * SDD^2
        
    # ADD THE STANDARD DEVIATION DISTANCE TO THE PLOT
    if(plot) {
  
      # DRAW THE SDD CIRCLE
      
      # DEFINE PLOT EXTENTS
      min.x <- min( (centre.xy[1] - SDD), min(destmat[,1]) )
      max.x <- max( (centre.xy[1] + SDD), max(destmat[,1]) )
      min.y <- min( (centre.xy[2] - SDD), min(destmat[,2]) )
      max.y <- max( (centre.xy[2] + SDD), max(destmat[,2]) )
      
      
      plot(1, type="n", asp=1, xlab="Easting (m)", ylab="Northing (m)", xlim=c(min.x, max.x), ylim=c(min.y, max.y))
      ellipse3(centre.xy[1], centre.xy[2], SDD, SDD, as_radians(0), col=6)
    
      if(plothv) {
        # DRAW HORIXONTAL AND VERTICAL LINES THROUGH CENTROID
        abline(h=centre.xy[2], col=1, lty = 2)
        abline(v=centre.xy[1], col=1, lty = 2)
      }
      if(plotdest) {
        # DRAW THE ACTIVITY LOCATIONS AS POINTS
        points(destmat)
      }
      if(plotcenter) {
        # ADD CENTROID POINT IN RED
        points(centre.xy[1],centre.xy[2], col=2)
      }          
    }
  
    # COMPUTE AND STORE COORDINATES FOR PLOTTING THE SDD CIRCLE      
    coordsSDD <- ellipse3(centre.xy[1], centre.xy[2], SDD, SDD, as_radians(0), col=6, pointsonly=TRUE)
    coordsSDD <- cbind(1, coordsSDD$x, coordsSDD$y)
    assign("OUTPUT_SDD", coordsSDD, pos=1)
    
    
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    outtabSDD <- cbind(id, OUTPUT_SDD)
    write.table(outtabSDD, sep=",", append=TRUE, file=filename, col.names=FALSE)
    
    
    
    if(box) {
      # COMPUTE AND ADD THE STANDARD DEVIATION OF THE X AND Y COORDINATES
      # CURRENTLY THE HOUSEHOLD IS NOT INCLUDED IN THIS COMPUTATION
      
      SDx <- sd(destmat[,1])
      SDy <- sd(destmat[,2])
    
      # PLOT STANDARD DEVIATION BOX CENTERED ON (EITHER HOUSEHOLD OR MEAN CENTER - DEPENDING ON WHICH IS SELECTED)
      segments( (centre.xy[1] - (SDx)), (centre.xy[2] + (SDy)), (centre.xy[1] + (SDx)), (centre.xy[2] + (SDy)), col=13)
      segments( (centre.xy[1] - (SDx)), (centre.xy[2] - (SDy)), (centre.xy[1] + (SDx)), (centre.xy[2] - (SDy)), col=13)
      segments( (centre.xy[1] - (SDx)), (centre.xy[2] + (SDy)), (centre.xy[1] - (SDx)), (centre.xy[2] - (SDy)), col=13)
      segments( (centre.xy[1] + (SDx)), (centre.xy[2] + (SDy)), (centre.xy[1] + (SDx)), (centre.xy[2] - (SDy)), col=13)
    
      # COMPUTE THE AREA OF THE SD BOX
      areabox <- SDx * SDy  
           
    }
    
    
    # PROVIDE THE SDD VALUE AS A RETURN PARAMETER TO THE CALLING FUNCTION
    result.sdd <- list("ID"=id, "calccentre"=calccentre, "Orig.x"=Orig.x, "Orig.y"=Orig.y, "CENTRE.x"=centre.xy[1], 
                       "CENTRE.y"=centre.xy[2], "SD.x"=SDx, "SD.y"=SDy, "SDD.radius"=SDD, "Box.area"=areabox, 
                       "SDD.area"=sddarea, "useWMC"=useWMC, "WeightPoints"=weightpoints)
    return(result.sdd)
     
  }
  else {
    # ERROR: TOO FEW ACTIVITY LOCATIONS: NEED >= 3
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 25
    if(verbose) {
      cat("\n\nWARNING: Not enough values to compute SDD.")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")
  }

}

