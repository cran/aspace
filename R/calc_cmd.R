"calc_cmd" <-
function(id=1, dist=100, points=NULL, verbose=FALSE) {

  #=======================================================
  #
  #  TITLE:     CENTRE OF MINIMUM DISTANCE (CMD) CALCULATOR 
  #  FUNCTION:  calc_cmd() 
  #  AUTHOR:    RON BULIUNG, RANDY BUI, TARMO K REMMEL
  #  DATE:      24 AUGUST 2023
  #  CALLS:     distances(), gridpts()
  #  NEEDS:     LIBRARIES: splancs
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE CMD; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE CMD POINTS TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO. USE cmdloc (coordinates) and cmdatt (attributes) 
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE 
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
  #
  #  OUTPUT:	
  #     ID  				UNIQUE CMD IDENTIFIER
  #		CMD.x				X-COORDINATE OF THE CENTRE OF MINIMUM DISTANCE
  #		CMD.y				Y-COORDINATE OF THE CENTRE OF MINIMUM DISTANCE
  #		Distance			HOLD DISTANCE VALUE BETWEEN I AND ITH ITERATIONS (metres)
  #		Number of Cells		HOLD NUMBER OF CELLS IN EACH GRID CREATED FOR EACH ITERATION
  #		cmdatt				ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		cmdloc				UNIQUE ID AND X,Y COORDINATES OF THE POINT FOR POST-PROCESSING AS SHAPEFILE
  #
  #  CALL:       garb <- calc_cmd(id=1, dist=100, points=activities)
  #
  #=======================================================

  # CREATE EMPTY OBJECTS
  x <- c() #HOLD X-COORD OF CMD FOR EACH ITERATION
  y <- c() #HOLD Y-COORD OF CMD FOR EACH ITERATION
  d <- c() #HOLD DISTANCE VALUE BETWEEN I AND ITH ITERATIONS
  n <- c() #HOLD ITERATION NUMBER
  cells <- c() #HOLD NUMBER OF CELLS IN EACH GRID CREATED FOR EACH ITERATION

  # INITIALIZE OBJECTS, COUNTERS, GRID SIZE
  i <- 1
  x[i] <- 0
  y[i] <- 0
  d[i] <- dist
  n[i] <- 0
  cells[i] <- 0
  dx <- 1 #INITIALIZE GRID SPACING IN X
  dy <- 1 #INITIALIZE GRID SPACING IN Y, LARGER NUMBER = MORE CELLS, HERE IT IS SET TO 1 CELLS IN X,Y

  # GENERATE MCP
  hpts <- grDevices::chull(points)
  MCP <- cbind(points[hpts,1], points[hpts,2])
  
  while (d[i] >= dist) {
    grid <- splancs::gridpts(MCP, dx, dy)
    M.CMD <- matrix(0,nrow=nrow(grid), ncol=3)
    for(j in 1:nrow(grid)) {
      coord.CMD <- grid[j,]
      sumdist.CMD <- sum(distances(centre.xy=coord.CMD, points))
      M.CMD[j,1] <- sumdist.CMD
      M.CMD[j,2] <- coord.CMD[1]
      M.CMD[j,3] <- coord.CMD[2]
    } # END FOR: j
    
    if (i >= 1) {
      order.CMD <- M.CMD[order(M.CMD[,1]),]
      CMD <- order.CMD[1,]
    } else (CMD <- M.CMD[1,])
				
    # DUMP CMD FOR EACH ITERATION
    x[i+1] <- CMD[2]
    y[i+1] <- CMD[3]

    # ESTIMATE DISTANCE BETWEEN CURRENT AND PREVIOUS CMD
    d[i+1] <- sqrt((x[i+1] - x[i])^2 + (y[i+1] - y[i])^2)
    n[i+1] <- dx
    cells[i+1] <- nrow(grid)

    rm(grid)
	i <- i+1
	dx <- dx+1
    dy <- dy+1
   
  } # END WHILE

  # PROCESS RESULTS, CMD HOLDS THE MINIMUM DISTANCE COORDINATES
  result <- cbind(n,round(x,2),round(y,2),round(d,2),cells)
  result <- as.data.frame(result[3:nrow(result),])
  result[,1] <- seq(1,nrow(result),1)
  colnames(result) <- c("n","X","Y","Dist","Cells")

  # CENTRE OF MINIMUM DISTANCE returned from simulation
  CMD <- cbind(result[nrow(result),2:5])
  colnames(CMD) <- c("X","Y","Dist","Cells")

  # STORE COORDINATES OF THE CMD
  coordsCMD <- cbind(CMD[,1], CMD[,2])

  # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  cmdloc <- as.data.frame(cbind(id, coordsCMD))
  colnames(cmdloc)=c("id","x","y")
	
  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.CMD <- list(id = id, points = points, CMD.x = CMD[,1], CMD.y = CMD[,2], result = result)

  # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
  # STORE CMD ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
  result.CMD <- list("id"=id, "CMD.x"=CMD[,1], "CMD.y"=CMD[,2], "Distance"=CMD[,3], "Number of Cells"=CMD[,4])
  result.CMD <- as.data.frame(result.CMD)
 
 if(verbose) {
    print(result.CMD)
  } # END IF
  
  # RETURN LIST WITH SIX ELEMENTS:
  # ELEMENT 1: A TYPE INDICATOR (BOX, SDD, OR SDE)
  # ELEMENT 2: DATE AND TIME THAT FUNCTION WAS RUN
  # ELEMENT 3: UNIQUE ID FOR DATASET (PASSED AS ARGUMENT TO THIS FUNCTION)
  # ELEMENT 4: boxloc IS A DATAFRAME REQUIRED FOR THE CONVERT.TO.SHAPEFILE FUNCTION
  # ELEMENT 5: r.BOX IS A LIST OBJECT REQUIRED FOR PLOTTING
  # ELEMENT 6: boxatt IS THE SD BOX ATTRIBUTES IN A DATA FRAME
  returnlist <- list("TYPE"="CMD", "DATE"=date(), "ID"=id, "LOCATIONS"=cmdloc, "FORPLOTTING"=r.CMD, "ATTRIBUTES"=result.CMD)
  return(returnlist)
    
} # END FUNCTION: calc_cmd
