"CF2PTS" <-
function(id=1, points1=NULL, points2=NULL, verbose=FALSE) {

  #=======================================================
  #
  #  TITLE:     CENTRAL FEATURE BETWEEN TWO POINT PATTERNS (CF2PTS) CALCULATOR 
  #  FUNCTION:  CF2PTS()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K REMMEL
  #  DATE:      24 AUGUST 2023
  #  CALLS:     distances()
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE CF2PTS; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE CF2PTS POINTS TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO. USE cf2ptsloc (coordinates) and cf2ptsatt (attributes) 
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE 
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
  #
  #  OUTPUT:	
  #     ID  	UNIQUE CF IDENTIFIER
  #		CF.x	X-COORDINATE OF THE CENTRAL FEATURE
  #		CF.y	Y-COORDINATE OF THE CENTRAL FEATURE
  #		cf2ptsatt	ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		cf2ptsloc	UNIQUE ID AND X,Y COORDINATES OF THE POINT FOR POST-PROCESSING AS SHAPEFILE
  #
  # CALL:       garb <- CF2PTS(id=1, filename="CF2PTS_Output.txt", points1=activities, points2=activities2, verbose=FALSE)
  #
  #=======================================================
  
  # DETERMINE THE CENTRAL FEATURE
  count.CF2PTS <- length(points2[,1])	
  M.CF2PTS <- matrix(0,nrow=count.CF2PTS,ncol=3)				

  for(i in 1:count.CF2PTS) {
    row.CF2PTS <- points2[i,]
    coord.CF2PTS <- c(row.CF2PTS[,1],row.CF2PTS[,2])

    dist.CF2PTS <- distances(centre.xy=coord.CF2PTS, points1, verbose=FALSE)
    sum.dist.CF2PTS <- sum(dist.CF2PTS)
					
    M.CF2PTS[i,1] <- sum.dist.CF2PTS
    M.CF2PTS[i,2] <- coord.CF2PTS[1]
    M.CF2PTS[i,3] <- coord.CF2PTS[2]
  } # END FOR: i

  order.CF2PTS <- M.CF2PTS[order(M.CF2PTS[,1]),]
  first.row.CF2PTS <- order.CF2PTS[1,]
	
  x.CF2PTS <- first.row.CF2PTS[2]
  y.CF2PTS <- first.row.CF2PTS[3]

  # STORE COORDINATES OF THE CF
  coordsCF2PTS <- cbind(x.CF2PTS, y.CF2PTS)
    
  # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  cf2ptsloc <- cbind(id, coordsCF2PTS)
  colnames(cf2ptsloc)=c("id","x","y")
  
  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.CF2PTS <- list(id = id, points1 = points1, points2 = points2, CF2PTS.x = x.CF2PTS, CF2PTS.y = y.CF2PTS)
    
  # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
  # STORE CF2PTS ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
  result.CF2PTS <- list("id"=id, "CF2PTS.x"=x.CF2PTS, "CF2PTS.y"=y.CF2PTS)
  result.CF2PTS<-as.data.frame(result.CF2PTS)
  if(verbose) {
    print(result.CF2PTS)
  } # END IF
  
  # BUILD RETURN LIST OBJECT
  returnlist <- list("TYPE"="CF2PTS", "DATE"=date(), "LOCATIONS"=cf2ptsloc, "FORPLOTTING"=r.CF2PTS, "ATTRIBUTES"=result.CF2PTS)
  return(returnlist)

} # END FUNCTION: CF2PTS
