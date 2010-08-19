"mean_centre" <-
function(id=1, filename="mean_centre_Output.txt", weighted=FALSE, weights=NULL, points=activities) {

  #=======================================================
  #
  #  TITLE:     MEAN CENTRE CALCULATOR
  #  FUNCTION:  mean_centre()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      August 19, 2010
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE MEAN CENTRE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE MEAN CENTRES ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE YOUR FILE TO CREATE SHAPEFILES AFTERWARDS.
  #
  #  OUTPUT:	
  #     ID  		UNIQUE MEAN CENTRE IDENTIFIER
  #		weighted    T|F SHOULD THE CENTRE BE WEIGHTED (WEIGHTED MEAN CENTER)
  #		weights		WEIGHTS (IF APPLIED)
  #		CENTRE.x	X-COORDINATE OF THE CENTRE
  #		CENTRE.y	Y-COORDINATE OF THE CENTRE
  #
  #=======================================================

  # STORE THE COUNT OF POINTS
  n <- dim(points)[1]
  
  # INITIALIZE centre.xy
  centre.xy <- NULL
  
  if(weighted) {
	# WEIGHT THE POINTS
	wt.x <- points[,1] * weights 
	wt.y <- points[,2] * weights
	
	# COMPUTE AND USE WEIGHTED MEAN CENTRE RATHER THAN USE SPECIFIED LOCATION AS CENTRE (WEIGHTED)
	WMC.x <- c( sum(wt.x) / sum(weights) )
	WMC.y <- c( sum(wt.y) / sum(weights) )    
	centre.xy[1] <- WMC.x
	centre.xy[2] <- WMC.y
   }
  else {
	# COMPUTE AND USE MEAN CENTRE RATHER THAN USE SPECIFIED LOCATION AS CENTRE (NON-WEIGHTED)
	meanx <- sum(points[,1])/n
	meany <- sum(points[,2])/n
	centre.xy[1] <- meanx
	centre.xy[2] <- meany
  } 

	# STORE COORDINATES OF THE MEAN CENTRE      
	coordsMC <- cbind(1, centre.xy[1], centre.xy[2])
    
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    outtabMC <- cbind(id, coordsMC)

    write.table(outtabMC, sep=",", append=TRUE, file=filename, col.names=FALSE)	
	
	# STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.mean <- list(id = id, points = points, weighted = weighted, weights = weights, 
	              CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2]) 
	assign("r.mean", r.mean, pos=1)
    
    # PROVIDE THE MEAN CENTRE COORDINATES AS A RETURN PARAMETER TO THE CALLING FUNCTION
    result.mean <- list("id"=id, "weighted"=weighted, "weights"=weights, "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2])
	return(result.mean)  
}

