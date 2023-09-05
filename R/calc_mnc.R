"calc_mnc" <-
function(id=1, weighted=FALSE, weights=NULL, points=NULL, verbose=FALSE) {
    
  #=======================================================
  #
  #  TITLE:     MEAN CENTRE CALCULATOR
  #  FUNCTION:  calc_mnc()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      March 28, 2011
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE MEAN CENTRE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE MEAN CENTRES TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION
  #             IS WRITTEN TO. USE meanloc (coordinates) and meanatt (attributes)
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
  #
  #  OUTPUT:
  #       ID  		UNIQUE MEAN CENTRE IDENTIFIER
  #		weighted    T|F SHOULD THE CENTRE BE WEIGHTED (WEIGHTED MEAN CENTER)
  #		weights		WEIGHTS (IF APPLIED)
  #		CENTRE.x	X-COORDINATE OF THE CENTRE
  #		CENTRE.y	Y-COORDINATE OF THE CENTRE
  #		meanatt		ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		meanloc		UNIQUE ID AND X,Y COORDINATES OF THE POINT FOR POST-PROCESSING AS SHAPEFILE
  #
  # CALL:           garb <- mean_centre(id=1, weighted=FALSE, weights=NULL, points=activities, verbose=FALSE)
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
  } # END IF
  else {
    # COMPUTE AND USE MEAN CENTRE RATHER THAN USE SPECIFIED LOCATION AS CENTRE (NON-WEIGHTED)
    meanx <- sum(points[,1])/n
    meany <- sum(points[,2])/n
    centre.xy[1] <- meanx
    centre.xy[2] <- meany
  } # END ELSE
    
  # STORE COORDINATES OF THE MEAN CENTRE
  coordsMC <- cbind(centre.xy[1], centre.xy[2])
    
  # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  meanloc <- cbind(id, coordsMC)
  colnames(meanloc)=c("id","x","y")
    
  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.mean <- list(id = id, points = points, weighted = weighted, weights = weights, CENTRE.x = centre.xy[1], CENTRE.y = centre.xy[2])
  
  # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
  # STORE MEAN CENTRE ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
  result.mean <- list("id"=id, "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2])
  result.mean <- as.data.frame(result.mean)
 
  if(verbose) {
    print(result.mean)
  } # END IF
      
  # RETURN LIST WITH SIX ELEMENTS:
  # ELEMENT 1: A TYPE INDICATOR (BOX, SDD, OR SDE)
  # ELEMENT 2: DATE AND TIME THAT FUNCTION WAS RUN
  # ELEMENT 3: UNIQUE ID FOR DATASET (PASSED AS ARGUMENT TO THIS FUNCTION)
  # ELEMENT 4: boxloc IS A DATAFRAME REQUIRED FOR THE CONVERT.TO.SHAPEFILE FUNCTION
  # ELEMENT 5: r.BOX IS A LIST OBJECT REQUIRED FOR PLOTTING
  # ELEMENT 6: boxatt IS THE SD BOX ATTRIBUTES IN A DATA FRAME
  returnlist <- list("TYPE"="MNC", "DATE"=date(), "ID"=id, "LOCATIONS"=meanloc, "FORPLOTTING"=r.mean, "ATTRIBUTES"=result.mean)
  return(returnlist)
    
} # END FUNCITON: calc_mnc
