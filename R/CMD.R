"CMD" <-
function(id=1, filename="CMD_Output.txt", CMD.npts=10000, points=activities) {

  #=======================================================
  #
  #  TITLE:     CENTRE OF MINIMUM DISTANCE (CMD) CALCULATOR 
  #  FUNCTION:  CMD()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      August 19, 2010
  #  CALLS:     distances(), mcp(), gridpts()
  #  NEEDS:     LIBRARIES: adehabitat, splancs
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE CMD; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE CMD ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE YOUR FILE TO CREATE SHAPEFILES AFTERWARDS.
  #
  #  OUTPUT:	
  #     ID  	UNIQUE CMD IDENTIFIER
  #		CMD.x	X-COORDINATE OF THE CENTRE OF MINIMUM DISTANCE
  #		CMD.y	Y-COORDINATE OF THE CENTRE OF MINIMUM DISTANCE
  #
  #=======================================================
  
  # COMPUTE THE CENTRE OF MINIMUM DISTANCE
  temp <- as.data.frame(cbind(1,points))
  temp[,1] <- as.factor(temp[,1])
  MCP <- (mcp(temp[,2:3], temp[,1], percent=100))
  MCP.extract <- cbind(X=MCP[,2],Y=MCP[,3])
  grid <- gridpts(MCP.extract,CMD.npts)	

  count.CMD <- length(grid[,1])	
  M.CMD <- matrix(0,nrow=count.CMD,ncol=3)				

  	for(i in 1:count.CMD) {
		coord.CMD <- grid[i,]

		dist.CMD <- distances(centre.xy=coord.CMD, points, verbose=FALSE)
		sum.dist.CMD <- sum(dist.CMD)
					
		M.CMD[i,1] <- sum.dist.CMD
		M.CMD[i,2] <- coord.CMD[1]
		M.CMD[i,3] <- coord.CMD[2]
	}

  order.CMD <- M.CMD[order(M.CMD[,1]),]
  first.row.CMD <- order.CMD[1,]
	
  x.CMD <- first.row.CMD[2]
  y.CMD <- first.row.CMD[3]

	# STORE COORDINATES OF THE CMD     
	coordsCMD <- cbind(1, x.CMD, y.CMD)
    
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    outtabCMD <- cbind(id, coordsCMD)

    write.table(outtabCMD, sep=",", append=TRUE, file=filename, col.names=FALSE)	
	
	# STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.CMD <- list(id = id, points = points, CMD.x = x.CMD, CMD.y = y.CMD) 
	assign("r.CMD", r.CMD, pos=1)
    
    # PROVIDE THE CMD COORDINATES AS A RETURN PARAMETER TO THE CALLING FUNCTION
    result.CMD <- list("id"=id, "CMD.x"=x.CMD, "CMD.y"=y.CMD)
	return(result.CMD)  
}