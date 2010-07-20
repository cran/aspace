"calc_box" <-
function(id=1, filename="BOX_Output.txt", centre.xy=NULL, calccentre=TRUE, weighted=FALSE, weights=NULL, CMD.npts=10000, points=activities, verbose=FALSE) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION BOX CALCULATOR
  #  FUNCTION:  calc_box()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      July 20, 2010
  #  CALLS:     distances(), mcp(), gridpts()
  #  NEEDS:     LIBRARIES: adehabitat, splancs, Hmisc
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE SD BOX; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             MULTIPLE SD BOXES ARE ADDED TO THE SAME FILE - KEEP IT UNIQUE!
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO.  USE THE FILE YOU CREATE TO DEVELOP SHAPEFILES AFTERWARDS.
  #
  #  ERROR:     1000  NO ERRORS DETECTED
  #               25  TOO FEW ACTIVITIES, NEED >= 3
  #               21  INVALID COMBINATION: calcentre=TRUE and centre.xy!=NULL
  #
  #  OUTPUT:	
  #     ID  		UNIQUE SDD IDENTIFIER
  #		calccentre	T|F SHOULD THE MEAN CENTRE BE USED
  #		weighted    T|F SHOULD THE CENTRE BE WEIGHTED (WEIGHTED MEAN CENTER)
  #		CENTRE.x	X-COORDINATE OF THE CENTRE
  #		CENTRE.y	Y-COORDINATE OF THE CENTRE
  #		central.x	X-COORDINATE OF CENTRAL FEATURE
  #		central.y	Y-COORDINATE OF CENTRAL FEATURE
  #		median.x	X-COORDINATE OF MEDIAN CENTRE
  #		median.y	Y-COORDINATE OF MEDIAN CENTRE
  #		CMD.x	    X-COORDINATE OF CENTRE OF MINIMUM DISTANCE
  #		CMD.y	    Y-COORDINATE OF CENTRE OF MINIMUM DISTANCE
  #		SD.x		ORTHOGONAL STD. DEV IN X-DIRECTION
  #		SD.y		ORTHOGONAL STD. DEV IN Y-DIRECTION
  #		Box.area	AREA OF ORTHOGONAL STD. DEV BOX IN COORDINATE UNITS
  #     NW.coord	NORTH-WEST CORNER OF SD BOX IN COORDINATE UNITS
  #     NE.coord	NORTH-EAST CORNER OF SD BOX IN COORDINATE UNITS
  #     SW.coord	SOUTH-WEST CORNER OF SD BOX IN COORDINATE UNITS
  #     SE.coord	SOUTH-EAST CORNER OF SD BOX IN COORDINATE UNITS
  #
  #=======================================================

  # SET DEPENDENCIES
  require(adehabitat)
  require(splancs)
  require(Hmisc)
  
  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000

  # STORE THE COUNT OF POINTS/CASES IN THE SOURCE DATASET
  n <- dim(points)[1]

  if(calccentre) {
    if(length(centre.xy) == 2) {
	  # ERROR: INVALID COMBINATION: calccentre=TRUE AND centre.xy!=NULL
      # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
      errorcode <- 21
      cat("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
	  cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
      return("ERROR")
	}
	else {
	  if(weighted) {
		# WEIGHT THE POINTS
		wt.x <- points[,1] * weights 
		wt.y <- points[,2] * weights
		
		# COMPUTE AND USE WEIGHTED MEAN CENTRE RATHER THAN USER SPECIFIED LOCATION AS CENTRE (WEIGHTED)
        WMC.x <- c( sum(wt.x) / sum(weights) )
        WMC.y <- c( sum(wt.y) / sum(weights) )    
        centre.xy[1] <- WMC.x
        centre.xy[2] <- WMC.y
       }
	  else {
        # COMPUTE AND USE MEAN CENTRE RATHER THAN USER SPECIFIED LOCATION AS CENTRE (NON-WEIGHTED)
        meanx <- sum(points[,1])/n
        meany <- sum(points[,2])/n
        centre.xy[1] <- meanx
        centre.xy[2] <- meany
      } 
    }
  }
  
  # COMPUTE THE MEDIAN CENTRE
  median.x <- median(points[,1])
  median.y <- median(points[,2])	
  
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

  CF <- c(x.CF,y.CF)			     

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

  CMD <- c(x.CMD,y.CMD)		      
  
  # INITIALIZE FUNCTION VARIABLE WITH PARAMETER VALUE
  dist <- distances(centre.xy, points)
  
  # TEST WHETHER A SUFFICIENT NUMBER OF POINTS WERE SUPPLIED
  if(length(dist) >= 3) {

	  if(weighted) {		
	  #PERFORM THE WEIGHTED STANDARD DEVIATION DISTANCE COMPUTATION (WEIGHTED SDD)
	  SDD <- sqrt(sum((weights*dist^2)/((sum(weights)) - 2) ) )
	  
	  # COMPUTE AND ADD THE STANDARD DEVIATION OF THE X AND Y COORDINATES
	  SDx <- sqrt(wtd.var(points[,1], weights))
	  SDy <- sqrt(wtd.var(points[,2], weights))
	  }
	  else {
	  # PERFORM THE STANDARD DEVIATION DISTANCE COMPUTATION (SDD)
	  SDD <- sqrt(sum(dist^2/(length(dist) - 2) ) )
	  
	  # COMPUTE AND ADD THE STANDARD DEVIATION OF THE X AND Y COORDINATES
	  SDx <- sd(points[,1])
	  SDy <- sd(points[,2])
	  }

	  # COMPUTE THE AREA OF THE SD BOX
	  areabox <- (2*SDx) * (2*SDy)  	
	 
	  # STORE THE COORDINATES OF EACH CORNER OF THE SD BOX IN SEPARATE OBJECTS   
	  
	  NW <- c((centre.xy[1] - (SDx)), (centre.xy[2] + (SDy)))
	  NE <- c((centre.xy[1] + (SDx)), (centre.xy[2] + (SDy)))
	  SW <- c((centre.xy[1] - (SDx)), (centre.xy[2] - (SDy)))
	  SE <- c((centre.xy[1] + (SDx)), (centre.xy[2] - (SDy)))
	  box.points <- rbind(NW, NE, SE, SW)
	  coordsBOX <- cbind(1, box.points[,1], box.points[,2])
    
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    outtabBOX <- cbind(id, coordsBOX)
    write.table(outtabBOX, sep=",", append=TRUE, file=filename, col.names=FALSE)
	
    # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.BOX <- list(id = id, points = points, calccentre = calccentre, weighted = weighted, weights = weights, CENTRE.x = centre.xy[1], 
	              CENTRE.y = centre.xy[2], central.x = CF[1], central.y = CF[2], median.x = median.x, median.y = median.y, CMD.x = CMD[1], CMD.y = CMD[2], 
				  SDD = SDD, SDx = SDx, SDy = SDy, Box.area = areabox, NW.coord = NW, NE.coord = NE, SW.coord = SW, SE.coord = SE)
	assign("r.BOX", r.BOX, pos=1)
	
    # PROVIDE THE BOX AREA AND ITS EXTENT AS A RETURN PARAMETER TO THE CALLING FUNCTION
    result.box <- list("id"=id, "calccentre"=calccentre, "weighted" = weighted, "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2],
					   "central.x"=CF[1], "central.y"=CF[2], "median.x"=median.x, "median.y"=median.y, "CMD.x"=CMD[1], 
					   "CMD.y"=CMD[2], "SD.x"=SDx, "SD.y"=SDy, "Box.area"=areabox, "NW.coord"=NW, "NE.coord"=NE, "SW.coord"=SW, "SE.coord"=SE)   
    return(result.box)
	
  }
  else {
    # ERROR: TOO FEW POINTS: NEED >= 3
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 25
    if(verbose) {
      cat("\n\nWARNING: Not enough values to compute SDD.")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")
  }
 
}