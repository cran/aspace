"calc_mdc" <-
function(id=1, points=NULL, verbose=FALSE) {
    
  #=======================================================
  #
  #  TITLE:     MEDIAN CENTRE CALCULATOR
  #  FUNCTION:  calc_mdc()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      24 AUGUST 2023
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE MEDIAN CENTRE; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE MEDIAN CENTRES TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION
  #             IS WRITTEN TO. USE medianloc (coordinates) and medianatt (attributes)
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
  #
  #  OUTPUT:
  #     ID  		UNIQUE MEDIAN CENTRE IDENTIFIER
  #		median.x	X-COORDINATE OF THE MEDIAN CENTRE
  #		median.y	Y-COORDINATE OF THE MEDIAN CENTRE
  #		medianatt	ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		medianloc	UNIQUE ID AND X,Y COORDINATES OF THE POINT FOR POST-PROCESSING AS SHAPEFILE
  #
  # CALL:           garb <- calc_mdc(id=1, points=activities, verbose=FALSE)
  #
  #=======================================================
    
  # COMPUTE THE MEDIAN CENTRE
  median.x <- median(points[,1])
  median.y <- median(points[,2])
    
  # STORE COORDINATES OF THE MEDIAN CENTRE
  coordsMC <- cbind(median.x, median.y)
    
  # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
  # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
  medianloc <- cbind(id, coordsMC)
  colnames(medianloc)=c("id","x","y")
    
  # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
  r.median <- list(id = id, points = points, median.x = median.x, median.y = median.y)
    
  # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
  # STORE MEDIAN CENTRE ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
  result.median <- list("id"=id, "median.x"=median.x, "median.y"=median.y)
  result.median <- as.data.frame(result.median)
 
  if(verbose) {
    print(result.median)
  } # END IF

  # RETURN LIST WITH SIX ELEMENTS:
  # ELEMENT 1: A TYPE INDICATOR (BOX, SDD, OR SDE)
  # ELEMENT 2: DATE AND TIME THAT FUNCTION WAS RUN
  # ELEMENT 3: UNIQUE ID FOR DATASET (PASSED AS ARGUMENT TO THIS FUNCTION)
  # ELEMENT 4: boxloc IS A DATAFRAME REQUIRED FOR THE CONVERT.TO.SHAPEFILE FUNCTION
  # ELEMENT 5: r.BOX IS A LIST OBJECT REQUIRED FOR PLOTTING
  # ELEMENT 6: boxatt IS THE SD BOX ATTRIBUTES IN A DATA FRAME
  returnlist <- list("TYPE"="MDC", "DATE"=date(), "ID"=id, "LOCATIONS"=medianloc, "FORPLOTTING"=r.median, "ATTRIBUTES"=result.median)
  return(returnlist)

} # END FUNCTION: median_centre
