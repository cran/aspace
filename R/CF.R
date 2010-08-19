"CF" <-
function(id=1, filename="CF_Output.txt", points=activities) {

  #=======================================================
  #
  #  TITLE:     CENTRAL FEATURE (CF) CALCULATOR 
  #  FUNCTION:  CF()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      August 19, 2010
  #  CALLS:     distances()
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE CF; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE CFs ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE YOUR FILE TO CREATE SHAPEFILES AFTERWARDS.
  #
  #  OUTPUT:	
  #     ID  	UNIQUE CF IDENTIFIER
  #		CF.x	X-COORDINATE OF THE CENTRAL FEATURE
  #		CF.y	Y-COORDINATE OF THE CENTRAL FEATURE
  #
  #=======================================================
  
  # DETERMINE THE CENTRAL FEATURE
  count.CF <- length(points[,1])	
  M.CF <- matrix(0,nrow=count.CF,ncol=3)				

  	for(i in 1:count.CF) {
		row.CF <- points[i,]
		coord.CF <- c(row.CF[,1],row.CF[,2])

		dist.CF <- distances(centre.xy=coord.CF, points, verbose=FALSE)
		sum.dist.CF <- sum(dist.CF)
					
		M.CF[i,1] <- sum.dist.CF
		M.CF[i,2] <- coord.CF[1]
		M.CF[i,3] <- coord.CF[2]
	}

  order.CF <- M.CF[order(M.CF[,1]),]
  first.row.CF <- order.CF[1,]
	
  x.CF <- first.row.CF[2]
  y.CF <- first.row.CF[3]

	# STORE COORDINATES OF THE CF     
	coordsCF <- cbind(1, x.CF, y.CF)
    
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    outtabCF <- cbind(id, coordsCF)

    write.table(outtabCF, sep=",", append=TRUE, file=filename, col.names=FALSE)	
	
	# STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.CF <- list(id = id, points = points, CF.x = x.CF, CF.y = y.CF) 
	assign("r.CF", r.CF, pos=1)
    
    # PROVIDE THE CF COORDINATES AS A RETURN PARAMETER TO THE CALLING FUNCTION
    result.CF <- list("id"=id, "CF.x"=x.CF, "CF.y"=y.CF)
	return(result.CF)  
}