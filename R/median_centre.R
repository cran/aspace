"median_centre" <-
function(id=1, filename="median_centre_Output.txt", points=activities) {

  #=======================================================
  #
  #  TITLE:     MEDIAN CENTRE CALCULATOR
  #  FUNCTION:  median_centre()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      August 19, 2010
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE MEDIAN CENTRE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE MEDIAN CENTRES ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE YOUR FILE TO CREATE SHAPEFILES AFTERWARDS.
  #
  #  OUTPUT:	
  #     ID  		UNIQUE MEDIAN CENTRE IDENTIFIER
  #		median.x	X-COORDINATE OF THE MEDIAN CENTRE
  #		median.y	Y-COORDINATE OF THE MEDIAN CENTRE
  #
  #=======================================================
  
    # COMPUTE THE MEDIAN CENTRE
	median.x <- median(points[,1])
	median.y <- median(points[,2])
  
	# STORE COORDINATES OF THE MEDIAN CENTRE      
	coordsMC <- cbind(1, median.x, median.y)
    
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    outtabMC <- cbind(id, coordsMC)

    write.table(outtabMC, sep=",", append=TRUE, file=filename, col.names=FALSE)	
	
	# STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.median <- list(id = id, points = points, median.x = median.x, median.y = median.y) 
	assign("r.median", r.median, pos=1)
    
    # PROVIDE THE MEDIAN CENTRE COORDINATES AS A RETURN PARAMETER TO THE CALLING FUNCTION
    result.median <- list("id"=id, "median.x"=median.x, "median.y"=median.y)
	return(result.median)  
}