"calc_box" <-
function(id=1, centre.xy=NULL, calccentre=TRUE, weighted=FALSE, weights=NULL, points=NULL, verbose=FALSE) {

  #=======================================================
  #
  #  TITLE:     STANDARD DEVIATION BOX CALCULATOR
  #  FUNCTION:  calc_box()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      25 AUGUST 2023
  #  CALLS:     distances()
  #  NEEDS:     LIBRARIES: NA (Originally needed Hmisc)
  #  NOTES:     USE THE id PARAMETER TO SPECIFY A UNIQUE IDENTIFIER FOR
  #             THE SD BOX; THIS VALUE IS ADDED TO THE OUTPUT filename
  #             AS AN IDENTIFIER THAT CAN BE USED TO EXTRACT RECORDS WHEN 
  #             A USER EMBEDDS THE FUNCTION IN A LOOP TO GENERATE
  #             MULTIPLE SD BOXES TO THE SAME FILE.
  #             THE filename PARAMETER CONTROLS WHERE THE COORDINATE INFORMATION 
  #             IS WRITTEN TO. USE boxloc (coordinates) and boxatt (attributes) 
  #             TO PRODUCE SHAPEFILES USING THE CONVERT.TO.SHAPEFILE AND WRITE.SHAPEFILE 
  #             FUNCTIONS FROM THE SHAPEFILES LIBRARY.
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
  #		SD.x		ORTHOGONAL STD. DEV IN X-DIRECTION
  #		SD.y		ORTHOGONAL STD. DEV IN Y-DIRECTION
  #		Box.area	AREA OF ORTHOGONAL STD. DEV BOX IN COORDINATE UNITS
  #     NW.coord	NORTH-WEST CORNER OF SD BOX IN COORDINATE UNITS
  #     NE.coord	NORTH-EAST CORNER OF SD BOX IN COORDINATE UNITS
  #     SW.coord	SOUTH-WEST CORNER OF SD BOX IN COORDINATE UNITS
  #     SE.coord	SOUTH-EAST CORNER OF SD BOX IN COORDINATE UNITS
  #		boxatt		ATTRIBUTES ABOVE WRITTEN TO DATAFRAME FOR POST-PROCESSING AS SHAPEFILE
  #		boxloc		UNIQUE ID AND X,Y COORDINATES OF VERTICES FOR POST-PROCESSING INTO SD BOX SHAPEFILE
  #
  # CALL:           garg <- calc_box(id=1, points=activities)
  #
  #=======================================================
  
  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000

  # STORE THE COUNT OF POINTS/CASES IN THE SOURCE DATASET
  n <- dim(points)[1]

  if(calccentre) {
    if(length(centre.xy) == 2) {
	  # ERROR: INVALID COMBINATION: calccentre=TRUE AND centre.xy!=NULL
      # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
      errorcode <- 21
      warning("\n\nWARNING: Invalid combination: calccentre=TRUE and centre.xy!=NULL")
	  warning("\nERROR CODE: ", errorcode, "\n\n", sep="")
      return("ERROR")
	} # END IF
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
       } # END IF
	  else {
        # COMPUTE AND USE MEAN CENTRE RATHER THAN USER SPECIFIED LOCATION AS CENTRE (NON-WEIGHTED)
        meanx <- sum(points[,1])/n
        meany <- sum(points[,2])/n
        centre.xy[1] <- meanx
        centre.xy[2] <- meany
      } # END ELSE
    } # END ELSE
  } # END IF
  
  # INITIALIZE FUNCTION VARIABLE WITH PARAMETER VALUE
  dist <- distances(centre.xy, points)
  
  # TEST WHETHER A SUFFICIENT NUMBER OF POINTS WERE SUPPLIED
  if(length(dist) >= 3) {

    if(weighted) {
      # PERFORM THE WEIGHTED STANDARD DEVIATION DISTANCE COMPUTATION (WEIGHTED SDD)
      SDD <- sqrt(sum((weights*dist^2)/((sum(weights)) - 2) ) )
	  
	  # COMPUTE AND ADD THE STANDARD DEVIATION OF THE X AND Y COORDINATES
	  SDx <- sqrt(Hmisc::wtd.var(points[,1], weights))
	  SDy <- sqrt(Hmisc::wtd.var(points[,2], weights))
    } # END IF
    else {
	  # PERFORM THE STANDARD DEVIATION DISTANCE COMPUTATION (SDD)
	  SDD <- sqrt(sum(dist^2/(length(dist) - 2) ) )
	  
	  # COMPUTE AND ADD THE STANDARD DEVIATION OF THE X AND Y COORDINATES
	  SDx <- sd(points[,1])
	  SDy <- sd(points[,2])
    } # END ELSE

    # COMPUTE THE AREA OF THE SD BOX
    areabox <- (2*SDx) * (2*SDy)
	 
    # STORE THE COORDINATES OF EACH CORNER OF THE SD BOX IN SEPARATE OBJECTS
    NW <- cbind((centre.xy[1] - (SDx)), (centre.xy[2] + (SDy)))
    NE <- cbind((centre.xy[1] + (SDx)), (centre.xy[2] + (SDy)))
    SW <- cbind((centre.xy[1] - (SDx)), (centre.xy[2] - (SDy)))
    SE <- cbind((centre.xy[1] + (SDx)), (centre.xy[2] - (SDy)))
    box.points <- rbind(NW, NE, SE, SW)
    coordsBOX <- cbind(box.points[,1], box.points[,2])
    
    # DATA FRAME WITH COLUMNS IN ORDER ID, X-COORD, Y-COORD FOR CONVERT.TO.SHAPEFILE FUNCTION
    # CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    boxloc <- as.data.frame(cbind(id, coordsBOX))
	colnames(boxloc) <- c("id","x","y")
	
    # STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.BOX <- list(id = id, points = points, calccentre = calccentre, weighted = weighted, weights = weights, CENTRE.x = centre.xy[1], 
	              CENTRE.y = centre.xy[2], SDD = SDD, SDx = SDx, SDy = SDy, Box.area = areabox, NW.coord = NW, NE.coord = NE, 
				  SW.coord = SW, SE.coord = SE)

    # DATA FRAME OF ATTRIBUTES WITH FIRST COLUMN NAME "ID" FOR CONVERT.TO.SHAPEFILE FUNCTION
    # STORE SD BOX ATTRIBUTES INTO A DATA FRAME AND PRINTS RESULTS
    result.box <- list("id"=id, "calccentre"=calccentre, "weighted" = weighted, "CENTRE.x"=centre.xy[1], "CENTRE.y"=centre.xy[2],
					   "SD.x"=SDx, "SD.y"=SDy, "Box.area"=areabox, "NW.coord"=NW, "NE.coord"=NE, "SW.coord"=SW, "SE.coord"=SE)   
	result.box <- as.data.frame(result.box)
    
    if(verbose) {
	  print(result.box)
    } # END IF
        
    # RETURN LIST WITH FIVE ELEMENTS:
    # ELEMENT 1: A TYPE INDICATOR (BOX, SDD, OR SDE)
    # ELEMENT 2: DATE AND TIME THAT FUNCTION WAS RUN
    # ELEMENT 3: boxloc IS A DATAFRAME REQUIRED FOR THE CONVERT.TO.SHAPEFILE FUNCTION
    # ELEMENT 2: r.BOX IS A LIST OBJECT REQUIRED FOR PLOTTING
    # ELEMENT 3: boxatt IS THE SD BOX ATTRIBUTES IN A DATA FRAME
    returnlist <- list("TYPE"="BOX", "DATE"=date(), "LOCATIONS"=boxloc, "FORPLOTTING"=r.BOX, "ATTRIBUTES"=result.box)
    return(returnlist)
  }
  else {
    # ERROR: TOO FEW POINTS: NEED >= 3
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 25
    if(verbose) {
      warning("\n\nWARNING: Not enough values to compute SDD.")
      warning("\nERROR CODE: ", errorcode, "\n\n", sep="")
    } # END IF
    return("ERROR")
  } # END ELSE
 
} # END FUNCITON: calc_box
