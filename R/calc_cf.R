"calc_cf" <-
function(id=1, points=NULL, verbose=FALSE) {
    
  #=======================================================
  #
  #  TITLE:     CENTRAL FEATURE (CF) CALCULATOR
  #  FUNCTION:  calc_cf()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K REMMEL
  #  DATE:      29 AUGUST, 2023
  #  CALLS:     distances()
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE CF; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE CF POINTS TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION
  #             IS WRITTEN TO. USE cfloc (coordinates) and cfatt (attributes)
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
  #
  #  OUTPUT:
  #     ID  	UNIQUE CF IDENTIFIER
  #		CF.x	X-COORDINATE OF THE CENTRAL FEATURE
  #		CF.y	Y-COORDINATE OF THE CENTRAL FEATURE
  #		cfatt	ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		cfloc	UNIQUE ID AND X,Y COORDINATES OF THE POINT FOR POST-PROCESSING AS SHAPEFILE
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
  } # END FOR: i
    
  order.CF <- M.CF[order(M.CF[,1]),]
  first.row.CF <- order.CF[1,]
    
  x.CF <- first.row.CF[2]
  y.CF <- first.row.CF[3]
    
  # STORE COORDINATES OF THE CF
  coordsCF <- cbind(x.CF, y.CF)
    
  # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  cfloc <- as.data.frame(cbind(id, coordsCF))
  colnames(cfloc)=c("id","x","y")
    
  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.CF <- list(id = id, points = points, CF.x = x.CF, CF.y = y.CF)
    
  # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
  # STORE CF ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
  result.CF <- list("id"=id, "CF.x"=x.CF, "CF.y"=y.CF)
  result.CF <- as.data.frame(result.CF)
  
  if(verbose) {
    print(result.CF)
  } # END IF
 
  # RETURN LIST WITH SIX ELEMENTS:
  # ELEMENT 1: A TYPE INDICATOR (BOX, SDD, OR SDE)
  # ELEMENT 2: DATE AND TIME THAT FUNCTION WAS RUN
  # ELEMENT 3: UNIQUE ID FOR DATASET (PASSED AS ARGUMENT TO THIS FUNCTION)
  # ELEMENT 4: boxloc IS A DATAFRAME REQUIRED FOR THE CONVERT.TO.SHAPEFILE FUNCTION
  # ELEMENT 5: r.BOX IS A LIST OBJECT REQUIRED FOR PLOTTING
  # ELEMENT 6: boxatt IS THE SD BOX ATTRIBUTES IN A DATA FRAME
  returnlist <- list("TYPE"="CF", "DATE"=date(), "ID"=id, "LOCATIONS"=cfloc, "FORPLOTTING"=r.CF, "ATTRIBUTES"=result.CF)
  return(returnlist)
  
} # END FUNCTION: calc_cf
