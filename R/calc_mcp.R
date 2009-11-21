"calc_mcp" <-
function(id=1, points=activities, filename="MCP_Output.txt", verbose=FALSE, pct=100) {

  #=======================================================
  #
  #  TITLE:     MINIMUM CONVEX POLYGON (MCP) CALCULATOR 
  #  FUNCTION:  calc_mcp()
  #  AUTHOR:    RANDY BUI, RON BULIUNG, TARMO K. REMMEL
  #  DATE:      November 21, 2009
  #  CALLS:     mcp.area(), mcp()
  #  NEEDS:     LIBRARY: adehabitat
  #  NOTES:     COMPUTES THE MINIMUM CONVEX POLYGON FOR A SERIES OF POINT LOCATIONS
  #             ACCORDING TO THE ADEHABITAT FUNCTIONS.  THIS WRAPPER SIMPLY PACKAGES
  #             THE PLOTTING, AREA, AND MCP FUNCTIONALITY.  WITH VERBOSE TURNED ON,
  #             DETAILED MESSAGES REGARDING POTENTIAL ERRORS ARE PROVIDED.  
  #
  #             THE PERCENTAGE PARAMETER MUST BE (0 <= pct <= 100) AND GOVERNS BOTH
  #             THE CONSTRUCTION OF THE MCP AND THE AREA OF THE RESULTING POLYGON.
  #
  #             THE OUTPUT AREA IS IN SQUARE KILOMETRES, INPUT COORDINATES MUST BE METERS.
  #             TYPICALLY THE UTM PROJECTION WITH EASTING AND NORTHING COORDINATES 
  #             WOULD ENSURE THAT THIS CONSTRAINT IS MET.
  #
  #  ERROR:     1000  NO ERRORS DETECTED
  #               70  TOO MANY COLUMNS IN POINTS MATRIX
  #               71  TOO FEW COLUMNS IN POINTS MATRIX
  #              100  RANGE OF PERCENTAGE VIOLATED
  #
  #  CALL:      results <- calc.mcp(points=activities, verbose=FALSE, pct=100, plot=TRUE)
  #
  #=======================================================

  # ENSURE NECESSARY LIBRARIES
  require(adehabitat)
    
  # INITIALIZE ERROR CODE TO NO ERROR
  errorcode <- 1000
    
  if( (pct > 100) || (pct < 0) ) {
    # ERROR: PERCENTAGE OF POINTS VALIDITY CHECK
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 100
    if(verbose) {
      cat("\n\nWARNING: Supplied percentage must be between 0 and 100 (inclusive).")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")      
  }
     
  if(length(dim(points)) != 2) {
    # ERROR: TOO FEW COLUMNS IN POINTS COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 71
    if(verbose) {
      cat("\n\nWARNING: Provided points input matrix has fewer than 2 columns.")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")    
  }
  
  if(dim(points)[2] != 2) {
    # ERROR: TOO MANY COLUMNS IN POINTS COORDINATE MATRIX
    # SET DESCRIPTIVE ERROR CODE AND GIVE WARNING
    errorcode <- 70
    if(verbose) {
      cat("\n\nWARNING: Provided points input matrix has too many columns, only 2 are allowed.")
      cat("\nERROR CODE: ", errorcode, "\n\n", sep="")
    }
    return("ERROR")
  }
  else {
    # COMPUTE AND DRAW THE MINIMUM CONVEX POLYGON: PROVIDE AREA
    temp <- as.data.frame(cbind(1,points))
    temp[,1] <- as.factor(temp[,1])
    MCP <- (mcp(temp[,2:3], temp[,1], percent=pct))
	
	# COMPUTE THE AREA OF THE MCP AT THE SPECIFIED PERCENTAGE. CHANGE unin AND unout ARGUMENTS TO MATCH COORDINATE UNITS
    area <- mcp.area(temp[,2:3], temp[,1], percent=pct, unin="m", unout="km2", plotit=FALSE)
    }

	# COMPUTE AND STORE COORDINATES FOR PLOTTING THE MCP
    coordsMCP <- cbind(id, MCP)

	# CREATE ASCII OUTPUT FOR SHAPEFILE CREATION
    tmp <- coordsMCP[,2:4]
    outtabMCP <- cbind(id, tmp)
    write.table(outtabMCP, sep=",", append=TRUE, file=filename, col.names=FALSE)

	# STORE RESULTS INTO A LIST (REQUIRED FOR PLOT FUNCTION)
	r.MCP <- list(MCP = MCP, points = points, id = id, MCP.area = area, MCP.pct = pct)
	assign("r.MCP", r.MCP, pos=1)

	# PACKAGE RESULTS INTO A LIST AND RETURN TO CALLING FUNCTION
    mcp.result <- list("id"=id, "MCP.area"=area, "MCP.pct"=pct, "MCP.coords"=MCP)  
    return(mcp.result)
 }

